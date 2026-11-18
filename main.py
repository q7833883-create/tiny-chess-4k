import os,json,urllib.request
V=[0,100,320,335,500,950,0]
D=[[],[15,17],[-33,-31,-18,-14,14,18,31,33],[-17,-15,15,17],[-16,-1,1,16],[-17,-16,-15,-1,1,15,16,17]]
D+=[D[5]]
def F(f):
 a,t,c,e,h,*_=f.split();b=[0]*128;i=112
 for x in a:
  if x=='/':i-=24
  elif x.isdigit():i+=int(x)
  else:b[i]=(2*x.isupper()-1)*' PNBRQK'.index(x.upper());i+=1
 return b,2*(t=='w')-1,c.strip('-'),-1 if e=='-' else I(e),int(h)
def K(z):return tuple(z[0]),*z[1:4]
def I(s):return ord(s[0])-97+16*(int(s[1])-1)
def U(m):
 a,b,p=m
 return ''.join(chr(97+i%16)+str(i//16+1) for i in (a,b))+'pnbrqk'[p-1:p]
def A(b,k,s):
 for d in D[5]:
  j=k+d
  while not j&136 and not b[j]:j+=d
  if not j&136:
   p=b[j]*s
   if p==5 or p==3+(d in D[4]) or j==k+d and (p==6 or p==1 and d in (-15*s,-17*s)):return 1
 for d in D[2]:
  if not (k+d)&136 and b[k+d]==2*s:return 1
def G(z):
 b,s,c,e,h=z
 for i in range(128):
  if i&136 or b[i]*s<=0:continue
  p=abs(b[i])
  if p==1:
   for d in (16,32,15,17):
    j=i+d*s
    if j&136:continue
    if d in (16,32):
     if b[j] or d==32 and (i>>4!=(1 if s>0 else 6) or b[i+16*s]):continue
    elif b[j]*s>=0 and j!=e:continue
    for q in ((5,2,4,3) if j>>4 in (0,7) else (0,)):yield i,j,q
  else:
   for d in D[p]:
    j=i+d
    while not j&136 and b[j]*s<=0:
     yield i,j,0
     if b[j] or p in (2,6):break
     j+=d
   if p==6 and not A(b,i,-s):
    for r,d in zip('KQ' if s>0 else 'kq',(1,-1)):
     if r in c and not any(b[i+d*x] for x in range(1,3 if d==1 else 4)) and not A(b,i+d,-s) and not A(b,i+2*d,-s):yield i,i+2*d,0
def M(z,m):
 b,s,c,e,h=z;b=b[:];i,j,q=m;p=b[i];v=b[j];b[i]=0;b[j]=s*q if q else p
 if p*p==1 and j==e:b[j-16*s]=0
 if p*p==36:
  c=''.join(r for r in c if r.isupper()!=(s>0))
  if abs(j-i)==2:
   a=(i&112)+(7 if j>i else 0);b[(i+j)//2]=b[a];b[a]=0
 for a,r in zip((0,7,112,119),'QKqk'):
  if a in (i,j):c=c.replace(r,'')
 if A(b,b.index(6*s),-s):return
 return b,-s,c,(i+j)//2 if p*p==1 and abs(j-i)==32 else -1,0 if v or p*p==1 else h+1
W=[[V[p]+(7-abs(2*(i&7)-7)-abs(2*(i>>4)-7))*[-18,2,7,7,0,3,10][p]+(i>>4)*(9*(p==1)+2*(p==4)-80*(p==0)) for i in range(128)] for p in range(7)]
W+=[[-v for v in row[::-1]] for row in W[:0:-1]]
def E(z):
 b,s,*_=z;v=sum(W[p][i] for i,p in enumerate(b) if p)
 for t in (-1,1):
  if -5*t in b:
   i=b.index(6*t);j=i if t>0 else i^112
   v+=t*(W[0][j]+35*b[i+16*t-1:i+16*t+2].count(t))
 return s*v
def S(z,d,a,b,l=0):
 global N
 N+=1
 if N>Z:raise RuntimeError
 B,s,c,e,h=z;k=K(z);t=T.get((k,l));o=a;C=A(B,B.index(6*s),-s)
 if h>=100 or l and k in R:return -Q*s
 if t and t[0]>=d and (not t[2] or t[2]*(t[1]-(a,b)[t[2]>0])>=0):return t[1]
 if d<=0 and not any(M(z,m) for m in G(z)):return -29000+l if C else 0
 if d<=0 and not C:
  v=E(z)
  if v>=b:return v
  a=max(a,v)
 x=-30000;y=None;R.add(k)
 for m in sorted(G(z),key=lambda m:10**6*(bool(t) and m==t[3])+10*V[abs(B[m[1]])]-abs(B[m[0]])+H.get(m,0)+m[2]*100,reverse=1):
  if d<=0 and not C and not B[m[1]] and not m[2] and m[1]!=e:continue
  w=M(z,m)
  if not w:continue
  v=-S(w,d-1,-b,-a,l+1)
  if v>x:x,y=v,m
  if v>a:a=v
  if a>=b:H[m]=H.get(m,0)+max(d,0)**2;break
 R.remove(k)
 if not y:return -29000+l if C else 0 if d>0 else a
 if d>0:T[k,l]=d,x,(x>=b)-(x<=o),y
 return a
Z=50000
def best_move(fen,h=(),f=None):
 global N,T,R,H,Q
 N=0;T={};R=set();H={};z=F(f or fen)
 for u in h:
  R.add(K(z));z=M(z,(I(u[:2]),I(u[2:4]),' pnbrqk'.find(u[4:])))
 z=F(fen);Q=100*z[1]*(E(z)>75);L=[m for m in G(z) if M(z,m)]
 if not L:return
 x=L[0]
 for d in range(1,64):
  try:S(z,d,-30000,30000);x=T[(K(z),0)][3]
  except RuntimeError:break
 return U(x)
def run():
 u=os.environ['MAKEFUN_ENV_URL'].rstrip('/')
 def P(p,x):return json.load(urllib.request.urlopen(urllib.request.Request(u+p,json.dumps(x).encode(),{'Content-Type':'application/json'})))
 g=P('/game',{})
 while g['status']!='done':
  if g['status']=='over':g=P('/game',{});continue
  m=best_move(g['fen'],g['moves'],g['start_fen'])
  if m not in g['legal_moves']:
   b=F(g['fen'])[0];m=max(g['legal_moves'],key=lambda x:V[abs(b[I(x[2:4])])])
  g=P('/move',{'game':g['game'],'move':m})
if __name__=='__main__':run()
