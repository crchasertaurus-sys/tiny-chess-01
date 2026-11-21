import json,os,urllib.request
KN=(-33,-31,-18,-14,14,18,31,33);K8=(-17,-16,-15,-1,1,15,16,17)
BD=(-17,-15,15,17);RD=(-16,-1,1,16);CR={119:1,112:2,7:4,0:8}
DS={'N':KN,'B':BD,'R':RD};KK={'K':3,'k':12}
V=dict(zip('PNBRQK',(100,300,320,500,950,0)))
PR='QRBN';MT=100000;R=range(128)
B=['.']*128;T=[1];C=[0];E=[-1];ND=[0]
def fen(s):
 p=s.split();B[:]=['.']*128;q=0
 for ch in p[0]:
  if ch=='/':q+=8
  elif ch.isdigit():q+=int(ch)
  else:B[q]=ch;q+=1
 T[0]=p[1]=='w';C[0]=sum(1<<x for x,ch in enumerate('KQkq') if ch in p[2]);E[0]=(8-int(p[3][1]))*16+ord(p[3][0])-97 if p[3]!='-' else -1
def at(s,w):
 if any(0<=s+o<128 and B[s+o]==('P' if w else 'p') for o in ((15,17) if w else (-15,-17))):return 1
 for d in KN+K8:
  t=s+d
  if not t&0x88 and B[t]==('NK' if w else 'nk')[d not in KN]:return 1
 for ds,ps in ((BD,'BQ' if w else 'bq'),(RD,'RQ' if w else 'rq')):
  for d in ds:
   t=s+d
   while not t&0x88 and B[t]=='.':t+=d
   if not t&0x88 and B[t] in ps:return 1
 return 0
def gn():
 w=T[0];res=[];A=res.append
 for i in R:
  if i&0x88:continue
  pc=B[i]
  if pc=='.' or (pc<'a')!=w:continue
  u=pc.upper();r=i>>4
  if u=='P':
   d=-16 if w else 16;one=i+d;lr=1 if w else 6
   tg=[one] if B[one]=='.' else []
   if tg and r==7-lr and B[i+2*d]=='.':tg.append(i+2*d)
   for dd in (d-1,d+1):
    t=i+dd
    if not t&0x88 and (B[t]!='.' and (B[t]<'a')!=w or t==E[0]):tg.append(t)
   for t in tg:
    if r==lr and (B[t]!='.' or t==one):res+=[i|t<<7|pi<<14 for pi in range(1,5)]
    elif r!=lr:A(i|t<<7)
  else:
   for d in DS.get(u,K8):
    t=i+d
    while not t&0x88:
     tp=B[t]
     if tp=='.':A(i|t<<7)
     else:
      if (tp<'a')!=w:A(i|t<<7)
      break
     if u in 'NK':break
     t+=d
   if u=='K':
    for bit,sg in ((1,1),(2,-1)) if w else ((4,1),(8,-1)):
     emp=(i+sg,i+2*sg,i+3*sg)[:2+(sg<0)]
     if C[0]&bit and all(B[x]=='.' for x in emp) and not any(at(x,1-w) for x in (i,)+emp[:2]):A(i|(i+2*sg)<<7)
 return res
def mk(m):
 i,j,pi=m&127,m>>7&127,m>>14;pc=B[i];u=pc.upper();sv=(B[:],C[0],E[0]);E[0]=-1
 if u=='P':
  if j==sv[2] and B[j]=='.':B[j+16 if pc<'a' else j-16]='.'
  if abs(j-i)==32:E[0]=(i+j)//2
  B[j]=(PR if pc<'a' else 'qrbn')[pi-1] if pi else pc
 else:
  B[j]=pc
  if u=='K' and abs(j-i)==2:
   d=1 if j>i else -1;f=(3 if d>0 else 4)*d;B[i+d]=B[i+f];B[i+f]='.'
 B[i]='.';c=C[0]&~KK.get(pc,0);c&=~CR.get(i,0);c&=~CR.get(j,0)
 C[0]=c;T[0]=1-T[0];return sv
def un(sv):
 B[:]=sv[0];C[0]=sv[1];E[0]=sv[2];T[0]=1-T[0]
def ks(w):return B.index('K' if w else 'k')
def ev():
 s=0
 for i in R:
  if i&0x88 or B[i]=='.':continue
  pc=B[i];u=pc.upper();wr=7-(i>>4) if pc<'a' else i>>4;cc=14-abs(2*(i&7)-7)-abs(2*wr-7)
  b=wr*6 if u=='P' else cc*2 if u=='N' else 0
  s+=V[u]+b if pc<'a' else -V[u]-b
 return s if T[0] else -s
def ok(m):
 cap=B[m>>7&127]
 return -V[cap.upper()]*10+V[B[m&127].upper()]//10 if cap!='.' else 0
def sr(d,a,b):
 ND[0]+=1
 if ND[0]>120000:raise Exception
 if d<=0:return ev()
 legal=0
 for m in sorted(gn(),key=ok):
  u=mk(m)
  if at(ks(1-T[0]),T[0]):un(u);continue
  legal+=1;sc=-sr(d-1,-b,-a);un(u)
  if sc>a:a=sc
  if a>=b:break
 if not legal:return -MT if at(ks(T[0]),1-T[0]) else 0
 return a
def uc(m):
 i,j=m&127,m>>7&127
 return 'abcdefgh'[i&7]+str(8-(i>>4))+'abcdefgh'[j&7]+str(8-(j>>4))+(PR[(m>>14)-1].lower() if m>>14 else '')
def bm(f,lg):
 fen(f);mv=[]
 for m in gn():
  u=mk(m)
  if not at(ks(1-T[0]),T[0]):mv.append(m)
  un(u)
 if not mv:return lg[0]
 mv.sort(key=ok);bst=mv[0];ND[0]=0;a=-10*MT
 try:
  for m in mv:
   u=mk(m);sc=-sr(4,-10*MT,-a);un(u)
   if sc>a:a,bst=sc,m
 except Exception:pass
 return uc(bst)
def po(env,path,body):
 req=urllib.request.Request(env+path,data=json.dumps(body).encode())
 with urllib.request.urlopen(req) as r:return json.load(r)
def play(env):
 while 1:
  g=po(env,'/game',{})
  if g['status']=='done':return
  while g['status']=='your_move':
   try:g=po(env,'/move',{'game':g['game'],'move':bm(g['fen'],g['legal_moves'])})
   except Exception:g=po(env,'/move',{'game':g['game'],'move':g['legal_moves'][0]})
if __name__=='__main__':play(os.environ['MAKEFUN_ENV_URL'])
