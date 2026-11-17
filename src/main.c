/* NumApp - imitation de l'interface NumWorks (accueil, Calculs, Python simplifie, Parametres)
 * Toutes les donnees sont dans une structure sur la pile (pas de variables globales). */
#include <eadk.h>
#include <stdint.h>
#include <stdbool.h>

const char eadk_app_name[] __attribute__((section(".rodata.eadk_app_name"))) = "NumApp";
const uint32_t eadk_api_level __attribute__((section(".rodata.eadk_api_level"))) = 0;

/* ---------- couleurs ---------- */
#define RGB(r,g,b) ((uint16_t)((((r)>>3)<<11)|(((g)>>2)<<5)|((b)>>3)))
#define C_WHITE  RGB(255,255,255)
#define C_BLACK  RGB(0,0,0)
#define C_YELLOW RGB(255,183,52)
#define C_DGREY  RGB(70,70,70)
#define C_GREY   RGB(150,150,150)
#define C_LGREY  RGB(225,225,225)
#define C_BLUE   RGB(30,90,200)
#define C_SEL    RGB(214,228,255)
#define C_RED    RGB(200,30,30)
#define C_GREEN  RGB(60,170,80)

/* ---------- touches (index dans eadk_keyboard_scan) ---------- */
enum {
  K_LEFT=0,K_UP=1,K_DOWN=2,K_RIGHT=3,K_OK=4,K_BACK=5,K_HOME=6,
  K_SHIFT=12,K_ALPHA=13,K_XNT=14,K_VAR=15,K_TOOLBOX=16,K_BACKSPACE=17,
  K_EXP=18,K_LN=19,K_LOG=20,K_I=21,K_COMMA=22,K_POWER=23,
  K_SIN=24,K_COS=25,K_TAN=26,K_PI=27,K_SQRT=28,K_SQUARE=29,
  K_7=30,K_8=31,K_9=32,K_LPAR=33,K_RPAR=34,
  K_4=36,K_5=37,K_6=38,K_MUL=39,K_DIV=40,
  K_1=42,K_2=43,K_3=44,K_PLUS=45,K_MINUS=46,
  K_0=48,K_DOT=49,K_EE=50,K_ANS=51,K_EXE=52
};

#define CL 48
#define CH 5
#define PW 46
#define PR 14
#define BL 24
#define BW 48
#define NV 40

typedef struct { char n[12]; double d; int t; } Var;
typedef struct { double d; int t; } Val;   /* t: 0 int, 1 float, 2 bool */

typedef struct {
  uint64_t prev, ev; uint8_t hold[56];
  bool deg, sci; int bright;
  /* calculs */
  char hin[CH][CL]; char hout[CH][28]; int hn; char in[CL]; int inl; double ans;
  const char *ep; bool eerr;
  /* python */
  char pout[PR][PW]; uint16_t pcol[PR]; int pn; char pin[PW]; int pinl; char plast[PW];
  bool alpha, shift, inblk;
  char ltext[BL][BW]; int lind[BL]; int blkn;
  Var vars[NV]; int nv;
  const char *p; const char *perr; char emsg[56]; int flow;
} App;

/* ---------- petites fonctions utilitaires ---------- */
static int slen(const char *s){ int n=0; while(s[n]) n++; return n; }
static bool starts(const char *s,const char *p){ while(*p){ if(*s!=*p) return false; s++; p++; } return true; }
static bool seq(const char *a,const char *b){ while(*a && *a==*b){ a++; b++; } return *a==*b; }
static void scopy(char *d,const char *s,int max){ int i=0; while(s[i] && i<max-1){ d[i]=s[i]; i++; } d[i]=0; }
static void sappend(char *d,const char *s,int max){ int n=slen(d); scopy(d+n,s,max-n); }
static bool isd(char c){ return c>='0' && c<='9'; }
static bool isidst(char c){ return (c>='a'&&c<='z')||(c>='A'&&c<='Z')||c=='_'; }
static bool isidc(char c){ return isidst(c)||isd(c); }

static void litoa(long long n,char *o){
  char t[24]; int i=0; bool neg=n<0;
  unsigned long long u = neg ? (unsigned long long)(-(n+1))+1ULL : (unsigned long long)n;
  do { t[i++]=(char)('0'+(int)(u%10)); u/=10; } while(u);
  int k=0; if(neg) o[k++]='-';
  while(i) o[k++]=t[--i];
  o[k]=0;
}

/* ---------- maths sans libm ---------- */
#define PI   3.14159265358979323846
#define LN2  0.69314718055994530942
#define LN10 2.30258509299404568402
#define NAN_ __builtin_nan("")
#define INF_ __builtin_inf()

static double fabs_(double x){ return x<0 ? -x : x; }
static double p10(int e){ double r=1,b=10; bool neg=e<0; if(neg) e=-e; while(e){ if(e&1) r*=b; b*=b; e>>=1; } return neg ? 1.0/r : r; }
static double m_floor(double x){
  if(x!=x || fabs_(x)>9e15) return x;
  double f=(double)(long long)x; if(f>x) f-=1; return f;
}
static double m_sqrt(double x){
  if(x!=x || x<0) return NAN_;
  if(x==0 || x>1e308) return x;
  double g = x>1 ? x : 1;
  for(int i=0;i<100;i++){ double n=0.5*(g+x/g); if(fabs_(n-g)<=1e-16*n){ g=n; break; } g=n; }
  return g;
}
static double m_exp(double x){
  if(x!=x) return x;
  if(x>709) return INF_;
  if(x<-745) return 0;
  int k=(int)(x/LN2 + (x>=0?0.5:-0.5));
  double r=x-k*LN2, term=1, sum=1;
  for(int n=1;n<25;n++){ term*=r/n; sum+=term; }
  while(k>0){ sum*=2; k--; }
  while(k<0){ sum*=0.5; k++; }
  return sum;
}
static double m_ln(double x){
  if(x!=x || x<0) return NAN_;
  if(x==0) return -INF_;
  if(x>1e308) return x;
  int e=0;
  while(x>=2){ x*=0.5; e++; }
  while(x<1){ x*=2; e--; }
  double s=(x-1)/(x+1), s2=s*s, term=s, sum=0;
  for(int n=1;n<60;n+=2){ sum+=term/n; term*=s2; }
  return 2*sum + e*LN2;
}
static double m_pow(double a,double b){
  if(b==0) return 1;
  if(a==0) return b>0 ? 0 : INF_;
  double bi=(double)(long long)b;
  if(fabs_(b)<=2048 && bi==b){
    long long n=(long long)b; bool neg=n<0; if(neg) n=-n;
    double r=1,base=a;
    while(n){ if(n&1) r*=base; base*=base; n>>=1; }
    return neg ? 1.0/r : r;
  }
  if(a<0) return NAN_;
  return m_exp(b*m_ln(a));
}
static double m_sin(double x){
  double t=x/(2*PI); long long k=(long long)(t + (t>=0?0.5:-0.5));
  x-=k*2*PI;
  double term=x,sum=x,x2=x*x;
  for(int n=1;n<=20;n++){ term*=-x2/((2.0*n)*(2.0*n+1)); sum+=term; }
  return sum;
}
static double m_cos(double x){
  double t=x/(2*PI); long long k=(long long)(t + (t>=0?0.5:-0.5));
  x-=k*2*PI;
  double term=1,sum=1,x2=x*x;
  for(int n=1;n<=20;n++){ term*=-x2/((2.0*n-1)*(2.0*n)); sum+=term; }
  return sum;
}
static double clean(double v){ return fabs_(v)<1e-13 ? 0 : v; }

/* ---------- formatage des nombres (10 chiffres significatifs) ---------- */
static void fmt_num(double v,char *o,bool sci){
  if(v!=v){ scopy(o,"undef",32); return; }
  if(v>1e308){ scopy(o,"inf",32); return; }
  if(v<-1e308){ scopy(o,"-inf",32); return; }
  if(v==0){ o[0]='0'; o[1]=0; return; }
  int k=0; if(v<0){ o[k++]='-'; v=-v; }
  int e=0; double m=v;
  while(m>=10){ m/=10; e++; }
  while(m<1){ m*=10; e--; }
  long long D=(long long)(m*1e9+0.5);
  if(D>=10000000000LL){ D/=10; e++; }
  char dg[12];
  for(int i=9;i>=0;i--){ dg[i]=(char)('0'+(int)(D%10)); D/=10; }
  int nd=10; while(nd>1 && dg[nd-1]=='0') nd--;
  if(!sci && e>=-5 && e<10){
    if(e>=0){
      for(int i=0;i<=e;i++) o[k++] = i<nd ? dg[i] : '0';
      if(nd>e+1){ o[k++]='.'; for(int i=e+1;i<nd;i++) o[k++]=dg[i]; }
    } else {
      o[k++]='0'; o[k++]='.';
      for(int i=0;i<-e-1;i++) o[k++]='0';
      for(int i=0;i<nd;i++) o[k++]=dg[i];
    }
    o[k]=0;
  } else {
    o[k++]=dg[0];
    if(nd>1){ o[k++]='.'; for(int i=1;i<nd;i++) o[k++]=dg[i]; }
    o[k++]='E';
    litoa(e,o+k);
  }
}

/* ---------- dessin ---------- */
static void rect(int x,int y,int w,int h,uint16_t c){
  eadk_rect_t r; r.x=(uint16_t)x; r.y=(uint16_t)y; r.width=(uint16_t)w; r.height=(uint16_t)h;
  eadk_display_push_rect_uniform(r,c);
}
static void text(const char *s,int x,int y,bool large,uint16_t fg,uint16_t bg){
  eadk_point_t p; p.x=(uint16_t)x; p.y=(uint16_t)y;
  eadk_display_draw_string(s,p,large,fg,bg);
}
static void header(const char *title,const char *right){
  rect(0,0,320,18,C_YELLOW);
  text(title,8,2,false,C_DGREY,C_YELLOW);
  if(right) text(right,240,2,false,C_DGREY,C_YELLOW);
  rect(292,5,22,9,C_DGREY); rect(293,6,20,7,C_WHITE); rect(294,7,14,5,C_GREEN); rect(314,7,2,5,C_DGREY);
}

/* ---------- clavier ---------- */
static void poll(App *a){
  uint64_t s=eadk_keyboard_scan();
  uint64_t ev=s & ~a->prev;
  for(int k=0;k<53;k++){
    if((s>>k)&1){
      if(a->hold[k]<250) a->hold[k]++;
      bool rep = (k<=3 || k==K_BACKSPACE);
      if(rep && a->hold[k]>12 && ((a->hold[k]-12)%3)==0) ev |= (1ULL<<k);
    } else a->hold[k]=0;
  }
  a->prev=s; a->ev=ev;
}
#define EV(k) ((int)((a->ev>>(k))&1))

/* ======================= CALCULS ======================= */
static double c_expr(App *a);
static double c_unary(App *a);

static double c_primary(App *a){
  const char *p=a->ep;
  if(*p=='('){
    a->ep++; double v=c_expr(a);
    if(*a->ep==')') a->ep++;
    return v;
  }
  if(isd(*p) || *p=='.'){
    double mant=0; int fd=0, digs=0;
    while(isd(*p)){ mant=mant*10+(*p-'0'); p++; digs++; }
    if(*p=='.'){ p++; while(isd(*p)){ mant=mant*10+(*p-'0'); fd++; p++; digs++; } }
    if(digs==0){ a->eerr=true; a->ep=p; return 0; }
    int ex=0;
    if(*p=='E'){
      p++; bool neg=false;
      if(*p=='-'){ neg=true; p++; } else if(*p=='+') p++;
      if(!isd(*p)){ a->eerr=true; a->ep=p; return 0; }
      while(isd(*p)){ if(ex<1000) ex=ex*10+(*p-'0'); p++; }
      if(neg) ex=-ex;
    }
    a->ep=p;
    return mant * p10(ex-fd);
  }
  if(starts(p,"pi")){ a->ep+=2; return PI; }
  if(starts(p,"Ans")){ a->ep+=3; return a->ans; }
  int id=-1, len=0;
  if(starts(p,"sqrt(")){ id=0; len=5; }
  else if(starts(p,"sin(")){ id=1; len=4; }
  else if(starts(p,"cos(")){ id=2; len=4; }
  else if(starts(p,"tan(")){ id=3; len=4; }
  else if(starts(p,"ln(")){ id=4; len=3; }
  else if(starts(p,"log(")){ id=5; len=4; }
  else if(starts(p,"exp(")){ id=6; len=4; }
  if(id>=0){
    a->ep+=len; double x=c_expr(a);
    if(*a->ep==')') a->ep++;
    double r=0;
    switch(id){
      case 0: r=m_sqrt(x); break;
      case 1: r=clean(m_sin(a->deg ? x*PI/180 : x)); break;
      case 2: r=clean(m_cos(a->deg ? x*PI/180 : x)); break;
      case 3: {
        double xr = a->deg ? x*PI/180 : x;
        double c=m_cos(xr);
        r = fabs_(c)<1e-14 ? NAN_ : clean(m_sin(xr)/c);
        break; }
      case 4: r=m_ln(x); break;
      case 5: r=m_ln(x)/LN10; break;
      default: r=m_exp(x); break;
    }
    return r;
  }
  a->eerr=true;
  return 0;
}
static double c_power(App *a){
  double b=c_primary(a);
  if(*a->ep=='^'){ a->ep++; double e=c_unary(a); b=m_pow(b,e); }
  return b;
}
static double c_unary(App *a){
  if(*a->ep=='-'){ a->ep++; return -c_unary(a); }
  if(*a->ep=='+'){ a->ep++; return c_unary(a); }
  return c_power(a);
}
static double c_term(App *a){
  double v=c_unary(a);
  while(!a->eerr){
    char c=*a->ep;
    if(c=='*'){ a->ep++; v*=c_unary(a); }
    else if(c=='/'){ a->ep++; double d=c_unary(a); v = (d==0) ? NAN_ : v/d; }
    else if(c=='(' || isd(c) || c=='.' || c=='p' || c=='A' || (c>='a'&&c<='z')){ v*=c_power(a); }
    else break;
  }
  return v;
}
static double c_expr(App *a){
  double v=c_term(a);
  while(!a->eerr){
    char c=*a->ep;
    if(c=='+'){ a->ep++; v+=c_term(a); }
    else if(c=='-'){ a->ep++; v-=c_term(a); }
    else break;
  }
  return v;
}
/* retourne false si erreur de syntaxe */
static bool calc_eval(App *a,const char *s,double *out){
  a->ep=s; a->eerr=false;
  double v=c_expr(a);
  if(*a->ep) a->eerr=true;
  *out=v;
  return !a->eerr;
}
static const char *calc_key(int k){
  switch(k){
    case K_0:return "0"; case K_1:return "1"; case K_2:return "2"; case K_3:return "3"; case K_4:return "4";
    case K_5:return "5"; case K_6:return "6"; case K_7:return "7"; case K_8:return "8"; case K_9:return "9";
    case K_DOT:return "."; case K_EE:return "E"; case K_PLUS:return "+"; case K_MINUS:return "-";
    case K_MUL:return "*"; case K_DIV:return "/"; case K_LPAR:return "("; case K_RPAR:return ")";
    case K_POWER:return "^"; case K_SQUARE:return "^2"; case K_SQRT:return "sqrt("; case K_SIN:return "sin(";
    case K_COS:return "cos("; case K_TAN:return "tan("; case K_LN:return "ln("; case K_LOG:return "log(";
    case K_EXP:return "exp("; case K_PI:return "pi"; case K_ANS:return "Ans";
    default:return 0;
  }
}
static void calc_push(App *a,const char *expr,const char *res){
  if(a->hn==CH){
    for(int i=1;i<CH;i++){ scopy(a->hin[i-1],a->hin[i],CL); scopy(a->hout[i-1],a->hout[i],28); }
    a->hn=CH-1;
  }
  scopy(a->hin[a->hn],expr,CL); scopy(a->hout[a->hn],res,28); a->hn++;
}
/* retourne true si l'ecran doit etre redessine */
static bool calc_step(App *a){
  bool d=false;
  for(int k=0;k<53;k++){
    if(!EV(k)) continue;
    const char *s=calc_key(k);
    if(s){
      if(slen(a->in)+slen(s)<CL-1){ sappend(a->in,s,CL); a->inl=slen(a->in); }
      d=true;
    }
  }
  if(EV(K_BACKSPACE)){
    int n=slen(a->in);
    if(n>0){
      /* efface un mot de fonction entier comme "sin(" */
      int cut=1;
      if(n>=5 && starts(a->in+n-5,"sqrt(")) cut=5;
      else if(n>=4 && (starts(a->in+n-4,"sin(")||starts(a->in+n-4,"cos(")||starts(a->in+n-4,"tan(")||starts(a->in+n-4,"log(")||starts(a->in+n-4,"exp("))) cut=4;
      else if(n>=3 && (starts(a->in+n-3,"ln(")||starts(a->in+n-3,"Ans"))) cut=3;
      a->in[n-cut]=0;
    }
    d=true;
  }
  if(EV(K_UP) && a->hn>0){ scopy(a->in,a->hin[a->hn-1],CL); d=true; }
  if(EV(K_EXE) || EV(K_OK)){
    if(slen(a->in)>0){
      double v; char res[32];
      if(calc_eval(a,a->in,&v)){
        fmt_num(v,res,a->sci);
        if(v==v && fabs_(v)<=1e308) a->ans=v;
      } else scopy(res,"Erreur syntaxe",32);
      calc_push(a,a->in,res);
      a->in[0]=0;
    } else if(a->hn>0){
      scopy(a->in,"Ans",CL);
    }
    d=true;
  }
  return d;
}
static void calc_draw(App *a){
  rect(0,0,320,240,C_WHITE);
  header("Calculs", a->deg ? "DEG" : "RAD");
  for(int e=0;e<a->hn;e++){
    int slot=CH-a->hn+e, y=20+slot*40;
    text(a->hin[e],6,y+2,false,C_DGREY,C_WHITE);
    int len=slen(a->hout[e]);
    text(a->hout[e],312-10*len,y+18,true,C_BLACK,C_WHITE);
    rect(0,y+39,320,1,C_LGREY);
  }
  rect(0,220,320,1,C_GREY);
  char line[CL+2]; scopy(line,a->in,CL); sappend(line,"_",CL+2);
  int n=slen(line); const char *show=line; if(n>44) show=line+(n-44);
  text(show,6,223,false,C_BLACK,C_WHITE);
}

/* ======================= PYTHON ======================= */
static Val mk(double d,int t){ Val v; v.d=d; v.t=t; return v; }
static void pskip(App *a){ while(*a->p==' ') a->p++; }
static void perr_(App *a,const char *m){ if(!a->perr) a->perr=m; }
static bool truthy(Val v){ return v.d!=0; }
static bool kwd(App *a,const char *k){
  pskip(a); int n=slen(k);
  if(starts(a->p,k) && !isidc(a->p[n])){ a->p+=n; return true; }
  return false;
}
static Var *findvar(App *a,const char *n){
  for(int i=0;i<a->nv;i++) if(seq(a->vars[i].n,n)) return &a->vars[i];
  return 0;
}
static void setvar(App *a,const char *n,Val v){
  Var *x=findvar(a,n);
  if(!x){
    if(a->nv>=NV){ perr_(a,"MemoryError: trop de variables"); return; }
    x=&a->vars[a->nv++]; scopy(x->n,n,12);
  }
  x->d=v.d; x->t=v.t;
}
static Val p_or(App *a);

static Val binop(App *a,char op,Val x,Val y){
  bool ints = (x.t!=1 && y.t!=1);
  switch(op){
    case '+': return mk(x.d+y.d, ints?0:1);
    case '-': return mk(x.d-y.d, ints?0:1);
    case '*': return mk(x.d*y.d, ints?0:1);
    case '/':
      if(y.d==0){ perr_(a,"ZeroDivisionError: division by zero"); return mk(0,1); }
      return mk(x.d/y.d,1);
    case 'D':
      if(y.d==0){ perr_(a,"ZeroDivisionError: division by zero"); return mk(0,0); }
      return mk(m_floor(x.d/y.d), ints?0:1);
    case '%':
      if(y.d==0){ perr_(a,"ZeroDivisionError: modulo by zero"); return mk(0,0); }
      return mk(x.d - m_floor(x.d/y.d)*y.d, ints?0:1);
    default: /* ** */
      if(x.d==0 && y.d<0){ perr_(a,"ZeroDivisionError: 0 ** negative"); return mk(0,1); }
      return mk(m_pow(x.d,y.d), (ints && y.d>=0)?0:1);
  }
}
static Val p_atom(App *a){
  pskip(a);
  char c=*a->p;
  if(a->perr) return mk(0,0);
  if(isd(c) || c=='.'){
    double mant=0; int fd=0; bool fl=false;
    while(isd(*a->p)){ mant=mant*10+(*a->p-'0'); a->p++; }
    if(*a->p=='.'){ fl=true; a->p++; while(isd(*a->p)){ mant=mant*10+(*a->p-'0'); fd++; a->p++; } }
    int ex=0;
    if((*a->p=='e'||*a->p=='E') && (isd(a->p[1]) || ((a->p[1]=='-'||a->p[1]=='+') && isd(a->p[2])))){
      fl=true; a->p++; bool neg=false;
      if(*a->p=='-'){ neg=true; a->p++; } else if(*a->p=='+') a->p++;
      while(isd(*a->p)){ if(ex<1000) ex=ex*10+(*a->p-'0'); a->p++; }
      if(neg) ex=-ex;
    }
    return mk(mant*p10(ex-fd), fl?1:0);
  }
  if(c=='('){
    a->p++; Val v=p_or(a); pskip(a);
    if(*a->p==')') a->p++; else perr_(a,"SyntaxError: ')' attendu");
    return v;
  }
  if(c=='"' || c=='\''){ perr_(a,"TypeError: texte seulement dans print"); return mk(0,0); }
  if(isidst(c)){
    char id[16]; int n=0;
    while(isidc(*a->p)){ if(n<15) id[n++]=*a->p; a->p++; }
    id[n]=0;
    if(seq(id,"math") && *a->p=='.'){
      a->p++; n=0;
      while(isidc(*a->p)){ if(n<15) id[n++]=*a->p; a->p++; }
      id[n]=0;
    }
    if(seq(id,"True")) return mk(1,2);
    if(seq(id,"False")) return mk(0,2);
    pskip(a);
    if(*a->p=='('){
      a->p++; Val ar[3]; int na=0; pskip(a);
      if(*a->p!=')'){
        for(;;){
          Val v=p_or(a); if(na<3) ar[na++]=v;
          pskip(a);
          if(*a->p==','){ a->p++; continue; }
          break;
        }
      }
      if(*a->p==')') a->p++; else perr_(a,"SyntaxError: ')' attendu");
      if(a->perr) return mk(0,0);
      if(na<1){ perr_(a,"TypeError: argument manquant"); return mk(0,0); }
      double x=ar[0].d;
      if(seq(id,"abs")) return mk(fabs_(x),ar[0].t==2?0:ar[0].t);
      if(seq(id,"int")) return mk((double)(long long)x,0);
      if(seq(id,"float")) return mk(x,1);
      if(seq(id,"floor")) return mk(m_floor(x),0);
      if(seq(id,"ceil")) return mk(-m_floor(-x),0);
      if(seq(id,"round")){
        double sc = na>1 ? p10((int)ar[1].d) : 1;
        double y=x*sc, f=m_floor(y), df=y-f, r;
        if(df>0.5) r=f+1; else if(df<0.5) r=f; else r = (((long long)f)%2==0) ? f : f+1;
        if(na>1) return mk(r/sc,1);
        return mk(r,0);
      }
      if(seq(id,"sqrt")){ if(x<0){ perr_(a,"ValueError: math domain error"); return mk(0,1);} return mk(m_sqrt(x),1); }
      if(seq(id,"sin")) return mk(clean(m_sin(x)),1);
      if(seq(id,"cos")) return mk(clean(m_cos(x)),1);
      if(seq(id,"tan")) return mk(clean(m_sin(x)/m_cos(x)),1);
      if(seq(id,"exp")) return mk(m_exp(x),1);
      if(seq(id,"log") || seq(id,"log10")){
        if(x<=0){ perr_(a,"ValueError: math domain error"); return mk(0,1); }
        double r=m_ln(x);
        if(seq(id,"log10")) r/=LN10;
        else if(na>1){ if(ar[1].d<=0 || ar[1].d==1){ perr_(a,"ValueError: math domain error"); return mk(0,1);} r/=m_ln(ar[1].d); }
        return mk(r,1);
      }
      if(seq(id,"min") && na>1) return (ar[1].d<x) ? ar[1] : ar[0];
      if(seq(id,"max") && na>1) return (ar[1].d>x) ? ar[1] : ar[0];
      if(seq(id,"pow") && na>1) return binop(a,'P',ar[0],ar[1]);
      a->emsg[0]=0; sappend(a->emsg,"NameError: name '",56); sappend(a->emsg,id,56); sappend(a->emsg,"' non defini",56);
      perr_(a,a->emsg); return mk(0,0);
    }
    if(seq(id,"pi")) return mk(PI,1);
    if(seq(id,"e")) return mk(2.71828182845904523536,1);
    Var *v=findvar(a,id);
    if(v) return mk(v->d,v->t);
    a->emsg[0]=0; sappend(a->emsg,"NameError: name '",56); sappend(a->emsg,id,56); sappend(a->emsg,"' non defini",56);
    perr_(a,a->emsg); return mk(0,0);
  }
  perr_(a,"SyntaxError: invalid syntax");
  return mk(0,0);
}
static Val p_unary(App *a);
static Val p_pow(App *a){
  Val b=p_atom(a); pskip(a);
  if(a->p[0]=='*' && a->p[1]=='*'){ a->p+=2; Val e=p_unary(a); return binop(a,'P',b,e); }
  return b;
}
static Val p_unary(App *a){
  pskip(a);
  if(*a->p=='-'){ a->p++; Val v=p_unary(a); return mk(-v.d, v.t==2?0:v.t); }
  if(*a->p=='+'){ a->p++; return p_unary(a); }
  return p_pow(a);
}
static Val p_term(App *a){
  Val v=p_unary(a);
  while(!a->perr){
    pskip(a);
    if(a->p[0]=='/' && a->p[1]=='/'){ a->p+=2; Val y=p_unary(a); v=binop(a,'D',v,y); }
    else if(a->p[0]=='/'){ a->p++; Val y=p_unary(a); v=binop(a,'/',v,y); }
    else if(a->p[0]=='*'){ a->p++; Val y=p_unary(a); v=binop(a,'*',v,y); }
    else if(a->p[0]=='%'){ a->p++; Val y=p_unary(a); v=binop(a,'%',v,y); }
    else break;
  }
  return v;
}
static Val p_arith(App *a){
  Val v=p_term(a);
  while(!a->perr){
    pskip(a);
    if(*a->p=='+'){ a->p++; Val y=p_term(a); v=binop(a,'+',v,y); }
    else if(*a->p=='-'){ a->p++; Val y=p_term(a); v=binop(a,'-',v,y); }
    else break;
  }
  return v;
}
static Val p_cmp(App *a){
  Val x=p_arith(a); pskip(a);
  int op=0;
  if(starts(a->p,"==")){ op=1; a->p+=2; }
  else if(starts(a->p,"!=")){ op=2; a->p+=2; }
  else if(starts(a->p,"<=")){ op=3; a->p+=2; }
  else if(starts(a->p,">=")){ op=4; a->p+=2; }
  else if(*a->p=='<'){ op=5; a->p++; }
  else if(*a->p=='>'){ op=6; a->p++; }
  if(!op) return x;
  Val y=p_arith(a);
  bool r=false;
  switch(op){
    case 1: r=x.d==y.d; break; case 2: r=x.d!=y.d; break; case 3: r=x.d<=y.d; break;
    case 4: r=x.d>=y.d; break; case 5: r=x.d<y.d; break; default: r=x.d>y.d; break;
  }
  return mk(r?1:0,2);
}
static Val p_not(App *a){
  if(kwd(a,"not")){ Val v=p_not(a); return mk(truthy(v)?0:1,2); }
  return p_cmp(a);
}
static Val p_and(App *a){
  Val v=p_not(a);
  while(!a->perr && kwd(a,"and")){ Val y=p_not(a); v = truthy(v) ? y : v; }
  return v;
}
static Val p_or(App *a){
  Val v=p_and(a);
  while(!a->perr && kwd(a,"or")){ Val y=p_and(a); v = truthy(v) ? v : y; }
  return v;
}
static void vfmt(Val v,char *o){
  if(v.t==2){ scopy(o, v.d!=0 ? "True" : "False", 32); return; }
  if(v.t==0 && fabs_(v.d)<9e15){ litoa((long long)v.d,o); return; }
  fmt_num(v.d,o,false);
  if(v.d!=v.d){ scopy(o,"nan",32); return; }
  bool plain=true;
  for(int i=0;o[i];i++) if(!(isd(o[i])||o[i]=='-')) plain=false;
  if(plain) sappend(o,".0",32);
}

/* sortie console */
static void pout_line(App *a,const char *s,uint16_t col){
  if(a->pn==PR){
    for(int i=1;i<PR;i++){ scopy(a->pout[i-1],a->pout[i],PW); a->pcol[i-1]=a->pcol[i]; }
    a->pn=PR-1;
  }
  scopy(a->pout[a->pn],s,PW); a->pcol[a->pn]=col; a->pn++;
}
static void pout(App *a,const char *s,uint16_t col){
  int n=slen(s);
  if(n==0){ pout_line(a,"",col); return; }
  char buf[PW];
  while(n>0){
    int c = n>PW-1 ? PW-1 : n;
    for(int i=0;i<c;i++) buf[i]=s[i];
    buf[c]=0; pout_line(a,buf,col);
    s+=c; n-=c;
  }
}
static bool py_intr(App *a){
  (void)a;
  return (eadk_keyboard_scan()>>K_BACK)&1;
}

static void py_stmt(App *a,char *s,bool echo){
  a->p=s; a->perr=0;
  pskip(a);
  s=(char*)a->p;
  if(*s==0 || *s=='#') return;
  if(seq(s,"pass")) return;
  if(starts(s,"import ") || starts(s,"from ")) return;
  if(seq(s,"break")){ a->flow=1; return; }
  if(seq(s,"continue")){ a->flow=2; return; }
  if(starts(s,"print(")){
    a->p=s+6;
    char out[200]; out[0]=0; bool first=true;
    pskip(a);
    if(*a->p!=')'){
      for(;;){
        pskip(a);
        if(!first) sappend(out," ",200);
        first=false;
        if(*a->p=='"' || *a->p=='\''){
          char q=*a->p; a->p++;
          char t[2]; t[1]=0;
          while(*a->p && *a->p!=q){ t[0]=*a->p; sappend(out,t,200); a->p++; }
          if(*a->p==q) a->p++; else { perr_(a,"SyntaxError: guillemet manquant"); return; }
        } else {
          Val v=p_or(a); if(a->perr) return;
          char t[32]; vfmt(v,t); sappend(out,t,200);
        }
        pskip(a);
        if(*a->p==','){ a->p++; continue; }
        break;
      }
    }
    if(*a->p==')') a->p++; else { perr_(a,"SyntaxError: ')' attendu"); return; }
    pskip(a);
    if(*a->p && *a->p!='#'){ perr_(a,"SyntaxError: invalid syntax"); return; }
    pout(a,out,C_BLACK);
    return;
  }
  if(isidst(*s)){
    char id[16]; int n=0; const char *q=s;
    while(isidc(*q)){ if(n<15) id[n++]=*q; q++; }
    id[n]=0;
    const char *r=q; while(*r==' ') r++;
    char op=0;
    if(*r=='=' && r[1]!='='){ op='='; r++; }
    else if(r[0]=='*' && r[1]=='*' && r[2]=='='){ op='P'; r+=3; }
    else if(r[0]=='/' && r[1]=='/' && r[2]=='='){ op='D'; r+=3; }
    else if((*r=='+'||*r=='-'||*r=='*'||*r=='/'||*r=='%') && r[1]=='='){ op=*r; r+=2; }
    if(op){
      a->p=r;
      Val v=p_or(a); pskip(a);
      if(!a->perr && *a->p && *a->p!='#') perr_(a,"SyntaxError: invalid syntax");
      if(a->perr) return;
      if(op!='='){
        Var *x=findvar(a,id);
        if(!x){
          a->emsg[0]=0; sappend(a->emsg,"NameError: name '",56); sappend(a->emsg,id,56); sappend(a->emsg,"' non defini",56);
          perr_(a,a->emsg); return;
        }
        v=binop(a,op,mk(x->d,x->t),v);
        if(a->perr) return;
      }
      setvar(a,id,v);
      return;
    }
  }
  Val v=p_or(a); pskip(a);
  if(!a->perr && *a->p && *a->p!='#') perr_(a,"SyntaxError: invalid syntax");
  if(a->perr) return;
  if(echo){ char t[32]; vfmt(v,t); pout(a,t,C_BLACK); }
}
static bool py_cond(App *a,char *t){
  /* t pointe apres "if "/"elif "/"while " ; coupe le ':' final */
  char buf[BW]; scopy(buf,t,BW);
  int n=slen(buf);
  while(n>0 && buf[n-1]==' ') n--;
  if(n==0 || buf[n-1]!=':'){ perr_(a,"SyntaxError: ':' attendu"); return false; }
  buf[n-1]=0;
  a->p=buf;
  Val v=p_or(a); pskip(a);
  if(!a->perr && *a->p) perr_(a,"SyntaxError: invalid syntax");
  return truthy(v);
}
static void py_run(App *a,int lo,int hi,bool echo);

static void py_body(App *a,int lo,int hi){
  if(hi<=lo){ perr_(a,"IndentationError: bloc attendu"); return; }
  py_run(a,lo,hi,false);
}
static void py_run(App *a,int lo,int hi,bool echo){
  int ind=a->lind[lo];
  int i=lo;
  while(i<hi && !a->perr && !a->flow){
    char *s=a->ltext[i];
    if(a->lind[i]!=ind){ perr_(a,"IndentationError: unexpected indent"); return; }
    int j=i+1; while(j<hi && a->lind[j]>ind) j++;
    if(starts(s,"if ")){
      bool taken=false; int cur=i, curj=j;
      for(;;){
        char *t=a->ltext[cur]; bool run=false;
        if(starts(t,"else:")) run=!taken;
        else if(!taken){ run=py_cond(a,t+(starts(t,"if ")?3:5)); if(a->perr) return; }
        if(run){ taken=true; py_body(a,cur+1,curj); if(a->perr||a->flow) return; }
        i=curj;
        if(i<hi && a->lind[i]==ind && (starts(a->ltext[i],"elif ")||starts(a->ltext[i],"else:"))){
          cur=i; curj=i+1; while(curj<hi && a->lind[curj]>ind) curj++;
        } else break;
      }
      continue;
    }
    if(starts(s,"elif ") || starts(s,"else:")){ perr_(a,"SyntaxError: invalid syntax"); return; }
    if(starts(s,"while ")){
      long cnt=0;
      for(;;){
        bool c=py_cond(a,s+6); if(a->perr) return;
        if(!c) break;
        py_body(a,i+1,j);
        if(a->perr) return;
        if(a->flow==1){ a->flow=0; break; }
        if(a->flow==2) a->flow=0;
        if(((++cnt)&15)==0 && py_intr(a)){ perr_(a,"KeyboardInterrupt"); return; }
      }
      i=j; continue;
    }
    if(starts(s,"for ")){
      a->p=s+4; pskip(a);
      char id[16]; int n=0;
      while(isidc(*a->p)){ if(n<15) id[n++]=*a->p; a->p++; }
      id[n]=0;
      if(n==0 || !kwd(a,"in") || !kwd(a,"range")){ perr_(a,"SyntaxError: seul 'for x in range(..):'"); return; }
      pskip(a);
      if(*a->p!='('){ perr_(a,"SyntaxError: invalid syntax"); return; }
      a->p++;
      Val ar[3]; int na=0; pskip(a);
      for(;;){
        Val v=p_or(a); if(a->perr) return;
        if(na<3) ar[na++]=v;
        pskip(a);
        if(*a->p==','){ a->p++; continue; }
        break;
      }
      if(*a->p==')') a->p++; else { perr_(a,"SyntaxError: ')' attendu"); return; }
      pskip(a);
      if(*a->p!=':' ){ perr_(a,"SyntaxError: ':' attendu"); return; }
      long long start=0,stop=0,step=1;
      if(na==1) stop=(long long)ar[0].d;
      else { start=(long long)ar[0].d; stop=(long long)ar[1].d; if(na>2) step=(long long)ar[2].d; }
      if(step==0){ perr_(a,"ValueError: range() step nul"); return; }
      long cnt=0;
      for(long long k=start; step>0 ? k<stop : k>stop; k+=step){
        setvar(a,id,mk((double)k,0)); if(a->perr) return;
        py_body(a,i+1,j);
        if(a->perr) return;
        if(a->flow==1){ a->flow=0; break; }
        if(a->flow==2) a->flow=0;
        if(((++cnt)&15)==0 && py_intr(a)){ perr_(a,"KeyboardInterrupt"); return; }
      }
      i=j; continue;
    }
    if(j>i+1){ perr_(a,"IndentationError: unexpected indent"); return; }
    py_stmt(a,s,echo);
    i=i+1;
  }
}
static void py_exec(App *a,bool echo){
  a->perr=0; a->flow=0;
  if(a->blkn>0) py_run(a,0,a->blkn,echo);
  if(a->perr) pout(a,a->perr,C_RED);
  a->flow=0; a->perr=0;
}
/* ajoute la ligne saisie au bloc en cours (retire commentaires et espaces finaux) */
static bool py_addline(App *a,const char *raw){
  if(a->blkn>=BL) return false;
  int ind=0; while(raw[ind]==' ') ind++;
  char *d=a->ltext[a->blkn];
  int n=0; char q=0;
  for(int i=ind; raw[i] && n<BW-1; i++){
    char c=raw[i];
    if(q){ if(c==q) q=0; }
    else if(c=='"'||c=='\'') q=c;
    else if(c=='#') break;
    d[n++]=c;
  }
  while(n>0 && d[n-1]==' ') n--;
  d[n]=0;
  a->lind[a->blkn]=ind;
  a->blkn++;
  return true;
}
static const char *py_alpha(int k,bool shift){
  /* retourne une chaine statique a 2 caracteres */
  int idx=-1;
  if(k>=18&&k<=34) idx=k-18; else if(k>=36&&k<=40) idx=k-36+17; else if(k>=42&&k<=46) idx=k-42+22;
  if(idx<0) return 0;
  switch(idx){
    case 0: return shift?"A":"a"; case 1: return shift?"B":"b"; case 2: return shift?"C":"c"; case 3: return shift?"D":"d";
    case 4: return shift?"E":"e"; case 5: return shift?"F":"f"; case 6: return shift?"G":"g"; case 7: return shift?"H":"h";
    case 8: return shift?"I":"i"; case 9: return shift?"J":"j"; case 10: return shift?"K":"k"; case 11: return shift?"L":"l";
    case 12: return shift?"M":"m"; case 13: return shift?"N":"n"; case 14: return shift?"O":"o"; case 15: return shift?"P":"p";
    case 16: return shift?"Q":"q"; case 17: return shift?"R":"r"; case 18: return shift?"S":"s"; case 19: return shift?"T":"t";
    case 20: return shift?"U":"u"; case 21: return shift?"V":"v"; case 22: return shift?"W":"w"; case 23: return shift?"X":"x";
    case 24: return shift?"Y":"y"; case 25: return shift?"Z":"z"; default: return " ";
  }
}
static const char *py_key(int k,bool alpha,bool shift){
  if(alpha){
    const char *s=py_alpha(k,shift);
    if(s) return s;
    if(k==K_0) return "_";
    if(k==K_DOT) return ".";
    return 0;
  }
  if(shift){
    switch(k){
      case K_VAR: return "<"; case K_TOOLBOX: return ">"; case K_COMMA: return "!";
      case K_PLUS: return "%"; case K_DOT: return "_"; case K_MINUS: return "'";
      default: break;
    }
  }
  switch(k){
    case K_0:return "0"; case K_1:return "1"; case K_2:return "2"; case K_3:return "3"; case K_4:return "4";
    case K_5:return "5"; case K_6:return "6"; case K_7:return "7"; case K_8:return "8"; case K_9:return "9";
    case K_DOT:return "."; case K_COMMA:return ","; case K_PLUS:return "+"; case K_MINUS:return "-";
    case K_MUL:return "*"; case K_DIV:return "/"; case K_LPAR:return "("; case K_RPAR:return ")";
    case K_POWER:return "**"; case K_SQUARE:return "**2"; case K_SQRT:return "sqrt("; case K_SIN:return "sin(";
    case K_COS:return "cos("; case K_TAN:return "tan("; case K_LN:return "log("; case K_LOG:return "log10(";
    case K_EXP:return "exp("; case K_PI:return "pi"; case K_ANS:return " "; case K_I:return ":";
    case K_VAR:return "="; case K_TOOLBOX:return "\""; case K_XNT:return "x"; case K_EE:return "e";
    default:return 0;
  }
}
static void py_init(App *a){
  a->pn=0; a->pin[0]=0; a->pinl=0; a->alpha=false; a->shift=false; a->inblk=false; a->blkn=0;
  pout(a,"Python (version simplifiee)",C_BLUE);
  pout(a,"Alpha=lettres  Ans=espace  i=':'",C_GREY);
  pout(a,"Var='='  Toolbox='\"'  Shift+Var='<'",C_GREY);
  pout(a,"for/while/if/print, pas de def/listes",C_GREY);
}
static bool py_step(App *a){
  bool d=false;
  if(EV(K_SHIFT)){ a->shift=!a->shift; d=true; }
  if(EV(K_ALPHA)){ a->alpha=!a->alpha; d=true; }
  for(int k=0;k<53;k++){
    if(!EV(k)) continue;
    if(k==K_SHIFT||k==K_ALPHA) continue;
    const char *s=py_key(k,a->alpha,a->shift);
    if(s){
      if(a->pinl+slen(s)<PW-6){ sappend(a->pin,s,PW); a->pinl=slen(a->pin); }
      a->shift=false; d=true;
    }
  }
  if(EV(K_BACKSPACE)){
    int n=slen(a->pin);
    if(n>0){
      bool onlysp=true; for(int i=0;i<n;i++) if(a->pin[i]!=' ') onlysp=false;
      int cut=1;
      if(onlysp && n>=4) cut=4; else if(onlysp) cut=n;
      a->pin[n-cut]=0; a->pinl=n-cut;
    }
    d=true;
  }
  if(EV(K_UP) && !a->inblk){ scopy(a->pin,a->plast,PW); a->pinl=slen(a->pin); d=true; }
  if(EV(K_EXE) || EV(K_OK)){
    char echo[PW+8]; scopy(echo, a->inblk ? "... " : ">>> ", PW+8); sappend(echo,a->pin,PW+8);
    pout(a,echo,C_DGREY);
    int ind=0; while(a->pin[ind]==' ') ind++;
    int n=slen(a->pin); while(n>0 && a->pin[n-1]==' ') n--;
    bool empty=(n==0);
    if(!a->inblk){
      if(!empty){
        scopy(a->plast,a->pin,PW);
        a->blkn=0; py_addline(a,a->pin);
        if(n>0 && a->pin[n-1]==':' ){
          a->inblk=true;
          for(int i=0;i<ind+4 && i<20;i++) a->pin[i]=' ';
          a->pin[ind+4<20?ind+4:20]=0;
        } else { py_exec(a,true); a->blkn=0; a->pin[0]=0; }
      } else a->pin[0]=0;
    } else {
      if(empty){
        py_exec(a,false); a->inblk=false; a->blkn=0; a->pin[0]=0;
      } else {
        if(!py_addline(a,a->pin)){ pout(a,"Bloc trop long",C_RED); a->inblk=false; a->blkn=0; a->pin[0]=0; }
        else {
          int ni = (n>0 && a->pin[n-1]==':') ? ind+4 : ind;
          if(ni>20) ni=20;
          for(int i=0;i<ni;i++) a->pin[i]=' ';
          a->pin[ni]=0;
        }
      }
    }
    a->pinl=slen(a->pin);
    d=true;
  }
  return d;
}
static void py_draw(App *a){
  rect(0,0,320,240,C_WHITE);
  header("Console", a->alpha ? "ALPHA" : (a->shift ? "SHIFT" : 0));
  for(int i=0;i<a->pn;i++) text(a->pout[i],4,20+i*14,false,a->pcol[i],C_WHITE);
  rect(0,216,320,1,C_GREY);
  char line[PW+8]; scopy(line, a->inblk ? "... " : ">>> ", PW+8); sappend(line,a->pin,PW+8); sappend(line,"_",PW+8);
  int n=slen(line); const char *show=line; if(n>44) show=line+(n-44);
  text(show,4,220,false,C_BLACK,C_WHITE);
}

/* ======================= PARAMETRES ======================= */
#define NSET 4
static bool set_step(App *a,int *sel,int *page){
  bool d=false;
  if(*page==1){ if(EV(K_OK)||EV(K_EXE)||EV(K_BACK)||EV(K_LEFT)){ *page=0; d=true; } return d; }
  if(EV(K_UP)){ *sel=(*sel+NSET-1)%NSET; d=true; }
  if(EV(K_DOWN)){ *sel=(*sel+1)%NSET; d=true; }
  bool chg=EV(K_OK)||EV(K_EXE)||EV(K_RIGHT)||EV(K_LEFT);
  if(chg){
    int dir = EV(K_LEFT) ? -1 : 1;
    if(*sel==0) a->deg=!a->deg;
    else if(*sel==1) a->sci=!a->sci;
    else if(*sel==2 && (EV(K_LEFT)||EV(K_RIGHT))){
      a->bright+=dir; if(a->bright<0) a->bright=0; if(a->bright>15) a->bright=15;
      eadk_backlight_set_brightness((uint8_t)(a->bright*16));
    }
    else if(*sel==3 && !EV(K_LEFT)) *page=1;
    d=true;
  }
  return d;
}
static void set_draw(App *a,int sel,int page){
  rect(0,0,320,240,C_WHITE);
  if(page==1){
    header("A propos",0);
    text("NumApp - version de test 0.1",12,34,true,C_BLACK,C_WHITE);
    text("Imitation de l'interface NumWorks",12,70,false,C_DGREY,C_WHITE);
    text("Applications actives:",12,96,false,C_DGREY,C_WHITE);
    text("  Calculs, Python, Parametres",12,114,false,C_DGREY,C_WHITE);
    text("OK / Retour pour revenir",12,200,false,C_GREY,C_WHITE);
    return;
  }
  header("Parametres",0);
  const char *lab[NSET]={"Unite d'angle","Format resultat","Luminosite","A propos"};
  for(int i=0;i<NSET;i++){
    int y=20+i*40;
    if(i==sel) rect(0,y,320,38,C_SEL);
    uint16_t bg = i==sel ? C_SEL : C_WHITE;
    text(lab[i],12,y+11,true,C_BLACK,bg);
    char v[24]; v[0]=0;
    if(i==0) scopy(v,a->deg?"Degres":"Radians",24);
    else if(i==1) scopy(v,a->sci?"Scientifique":"Decimal",24);
    else if(i==2){ litoa(a->bright,v); sappend(v,"/15",24); }
    else scopy(v,">",24);
    text(v,308-7*slen(v),y+13,false,C_BLUE,bg);
    rect(0,y+39,320,1,C_LGREY);
  }
}

/* ======================= ACCUEIL ======================= */
static void icon(int i,int x,int y){
  /* x,y = coin haut-gauche d'une boite 40x40 */
  uint16_t col;
  switch(i){
    case 0: col=RGB(235,110,50); break; case 1: col=RGB(60,160,90); break; case 2: col=RGB(140,80,190); break;
    case 3: col=RGB(40,120,200); break; case 4: col=RGB(210,60,110); break; case 5: col=RGB(30,160,170); break;
    case 6: col=RGB(235,185,30); break; default: col=RGB(110,110,120); break;
  }
  rect(x,y,40,40,col);
  rect(x+2,y+2,36,36,col);
  switch(i){
    case 0: text("+-",x+10,y+11,true,C_WHITE,col); break;
    case 1: text("fx",x+10,y+11,true,C_WHITE,col); break;
    case 2: text("un",x+10,y+11,true,C_WHITE,col); break;
    case 3: rect(x+7,y+22,6,13,C_WHITE); rect(x+17,y+12,6,23,C_WHITE); rect(x+27,y+18,6,17,C_WHITE); break;
    case 4: text("%",x+15,y+11,true,C_WHITE,col); break;
    case 5: text("x=",x+10,y+11,true,C_WHITE,col); break;
    case 6: text("Py",x+10,y+11,true,C_BLACK,col); break;
    default: text("*",x+15,y+11,true,C_WHITE,col); break;
  }
}
static void home_draw(int sel){
  rect(0,0,320,240,C_WHITE);
  header("Applications",0);
  const char *nm[8]={"Calculs","Fonctions","Suites","Statistiques","Probabilites","Equations","Python","Parametres"};
  for(int i=0;i<8;i++){
    int cx=(i%3)*106+1, cy=18+(i/3)*74;
    if(i==sel){ rect(cx+2,cy+2,102,70,C_YELLOW); rect(cx+5,cy+5,96,64,C_WHITE); }
    icon(i,cx+33,cy+8);
    int len=slen(nm[i]);
    text(nm[i],cx+53-(len*7)/2,cy+52,false,C_BLACK,C_WHITE);
  }
}

/* ======================= PRINCIPAL ======================= */
enum { S_HOME, S_CALC, S_PY, S_SET };

int main(int argc,char *argv[]){
  (void)argc; (void)argv;
  App st; App *a=&st;
  { char *b=(char*)a; for(unsigned i=0;i<sizeof(App);i++) b[i]=0; }
  a->deg=true; a->sci=false;
  a->bright=eadk_backlight_brightness()/16; if(a->bright>15) a->bright=15; if(a->bright<0) a->bright=0;
  a->prev=eadk_keyboard_scan();
  int screen=S_HOME, sel=0, setsel=0, setpage=0;
  bool dirty=true;
  py_init(a);
  for(;;){
    poll(a);
    if(screen==S_HOME){
      if(EV(K_RIGHT)){ sel=(sel+1)%8; dirty=true; }
      if(EV(K_LEFT)){ sel=(sel+7)%8; dirty=true; }
      if(EV(K_DOWN)){ sel = sel+3<8 ? sel+3 : sel%3; dirty=true; }
      if(EV(K_UP)){ sel = sel-3>=0 ? sel-3 : (sel+6<8 ? sel+6 : sel+3); dirty=true; }
      if(EV(K_OK)||EV(K_EXE)){
        if(sel==0){ screen=S_CALC; dirty=true; }
        else if(sel==6){ screen=S_PY; dirty=true; }
        else if(sel==7){ screen=S_SET; setpage=0; dirty=true; }
      }
      if(EV(K_BACK)||EV(K_HOME)) return 0;
    } else if(screen==S_CALC){
      if(EV(K_BACK)||EV(K_HOME)){ screen=S_HOME; dirty=true; }
      else if(calc_step(a)) dirty=true;
    } else if(screen==S_PY){
      if(EV(K_BACK)||EV(K_HOME)){ screen=S_HOME; dirty=true; }
      else if(py_step(a)) dirty=true;
    } else {
      if(EV(K_HOME)){ screen=S_HOME; dirty=true; }
      else if(EV(K_BACK) && setpage==0){ screen=S_HOME; dirty=true; }
      else if(set_step(a,&setsel,&setpage)) dirty=true;
    }
    if(dirty){
      if(screen==S_HOME) home_draw(sel);
      else if(screen==S_CALC) calc_draw(a);
      else if(screen==S_PY) py_draw(a);
      else set_draw(a,setsel,setpage);
      dirty=false;
    }
    eadk_timing_msleep(20);
  }
}
