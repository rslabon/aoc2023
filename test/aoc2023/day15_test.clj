(ns aoc2023.day15-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")
(def puzzle-input "ts-,ckfx-,hg-,rhqh=9,qxcn-,pf=5,lrm-,lhgp=5,xf=7,cpk-,gt=4,lb=5,cj-,qd=6,jgz=7,qrx=4,drh-,gbbq=9,dgkp=2,dkn=4,cvb-,dt=2,xlq-,fnr=3,qh=3,zqrvs=9,jz=3,fcx-,szl=9,drc-,vkq-,mhlh-,gj-,ff=9,qv-,pjt-,xv-,vf=4,ljdk-,szl-,jff-,nvf=9,zvkg=1,sg-,trssr=6,qd=4,rphhlq=4,gl-,zbhd=7,mbdfxs=2,dhzd-,dgkp=1,pfgnk=9,cq-,bt=6,mqsf=5,hz-,qfb-,rxnvs-,glh-,lbkm=5,rxnbf=3,phm-,gn-,ccxk=5,rl-,lsh=5,zt=2,dkn-,lb=5,cs-,xlq-,nb=7,stt=6,xvcc-,tzgj-,fxd=8,rnbjz=2,vnnr=5,xj=9,htckf=6,zm-,qc-,vvdq-,jqh=6,qrx-,ms=5,trssr-,jt-,nkl=6,vnrx-,qtfk=4,mmc=6,nr-,psplc=2,mb=1,dv-,lv=7,gsr=4,rc=3,ffcq=8,nd=5,gfm=1,hzm=1,sz=1,cjv=8,kjj-,gb-,vqc-,ktg=5,kghs-,dh-,bm-,zrb-,drc-,bd-,svsqz=3,jk-,fdjc=5,hrc=4,hd=7,tht-,mh-,bh=7,qd-,jcr-,fk=2,rbb=7,bql-,fmnc-,ckc=3,bfxtcj-,cjt=8,hl=9,bh=1,drh-,dp-,bcml-,fxd-,glh-,bt=8,rn=2,sf=2,gjvvl=8,hp=9,xj-,rl=2,pvvrr=9,zm=6,vqc-,xgjmg-,tdl-,ckfx=6,cjt-,mmc=4,bnmzn-,pjt-,jf-,tdl-,bgx-,tcncr=4,qf-,rvzvt-,krckj-,fb-,bbk=7,xhnpk-,fjgr-,jks-,ghnt-,krckj=5,gx-,smcl=7,rbzz-,sg=3,dr=5,jq=2,qhxdp=2,dbp=3,kp=2,pnzh-,jcvp-,fgp-,rqg-,rlzk=1,bm=3,htfhlh-,hzm-,pb=4,nfht-,cs-,glh=1,qtfk-,dql-,vvdq=5,dgkp=7,hj-,hlbrz=2,trssr-,sfnjpz-,pmh-,lbkm-,fgp-,hlbrz-,lvg-,dttxng-,crm=6,nsb=8,sdj=5,kzdc-,vntc=2,rhqh-,fxl-,sv=2,tnm-,kzdc=4,pvvrr=9,fsx-,pvvrr=7,gfl-,ft-,vvtr=7,kqmxm-,jml=9,jv=3,bbn-,cjv-,ntbv-,ktg-,dp=8,bqrq-,zrb=7,xbk=9,xvcc-,fmnc-,bv=2,nq=9,xmv-,nxdfr-,rbzz-,fjc=6,gn-,qcx-,ghnt-,fmnc=4,pvb=1,bbdf=5,rnbjz-,drc-,rvhdj=5,fpkhl-,gtjn=5,bpc=6,xv-,htckf=7,jgq-,lz-,xlq-,cvb-,dz=2,lvg=2,qq=8,qrhm-,jzq=7,qrx=6,rm-,zz=4,fq=8,kjj-,djz=3,dcv-,rlzk=2,tltpx=7,kzdc-,mbs=4,vt=3,tgzc-,tbtnhh=5,fgp=4,rmtvzc=5,jjk-,rmtvzc=4,xv-,jk-,pjq=5,kgrz-,mhlh-,hd-,hd-,qrhm=6,tvdl-,zpkg=4,dql=2,fx=3,jcr=5,ksxh-,xgjmg=2,qf-,hlz=3,jvt=5,qhxdp-,hlz=3,ft-,vntc=4,jks=8,qh-,vkq-,kqmxm=6,qfb-,jjk-,xgjmdq-,gl-,qh=4,tzgj-,qd=9,vnnr-,fl-,crfqnr=2,mvkj=6,qcpgc=2,pgx=7,nzzm=6,nqtvnt-,ckz-,np=2,tv-,df=7,vcl-,nxdfr=5,ln=6,xv-,dp-,xmv-,pvvrr=8,rbzz-,kjj-,qmd-,xhnpk-,nr=3,hzm=7,plsvvv=9,kbll-,lrm=8,nqtvnt-,kzdc-,fsx-,svg=8,gl=4,cpmg=2,ljtf-,rmbp=5,sl=3,lgql=7,mpfq-,gj=7,fx-,tvdl-,jb=8,fmnc-,ftkx-,td=3,cg-,rm-,kgrz-,kn=6,slgcl-,kp-,nxdfr=1,nxq=4,dsv-,hrc=3,cbr-,vt=4,bbdf=4,sz=5,tht-,kr-,cjv-,hg=5,fg-,trssr=1,ndz=4,gxg=5,hstk=1,tgzc=1,rt=1,dnl=4,mb=8,rmbp=1,mxzx-,cpp=3,fl-,czb-,glh=1,fbv-,gt=6,crc-,kq-,pmh-,fpk=9,tbtnhh-,pm=3,rfkc-,bf-,dt=9,jmh=6,gkk-,rnzc-,hz-,czch=8,pjt=8,vhp-,jcvp=6,dtg-,djz-,rb-,tq-,xj=2,pjq=6,bh=6,kzrm-,vnrx-,jmh=6,hs=9,rkzkmk-,crc=5,vt-,kpnvd-,njv=4,pjq-,dt=8,jz=3,tvdl=8,rxbhq-,slm=6,ktkt=9,pjq-,kzrm-,nf=1,pmkn=5,bxls-,jmh=2,tht=5,jktc=9,cg-,hl=2,gkk-,ckfx=6,cctp=7,xf=7,bgx-,kmx-,tzv=6,bs=6,qrhm=4,ndbcd=1,lb-,frd=8,cjv=1,gxp-,rx-,bh-,gf-,zl-,zldz-,zz-,df=5,vcgq=4,jj=4,bqrq-,nsdzj=1,srqc-,hstk-,fv=2,tcncr=5,fst-,fqzh-,dl-,gfm=6,ddh-,rt-,bql=9,dttxng-,ff=4,tt=1,zdh=9,vntc=9,qbk-,mhlh-,fsx-,zvtxf-,rf=9,hbs=5,gbbq=5,cs-,rpjz=6,lhnn=7,lz-,gt=8,qg-,qjz=2,kn-,kgrz-,kcj=2,kn=6,bqrq-,hm=1,mbls-,vqc=5,hg-,nss=6,vr-,dnl-,qfb-,zvkg=7,ch-,dcv-,dql=5,nsdzj=8,jvt-,sxl-,zllr=9,mztd=6,cvb-,km-,dcq=7,km=1,jt=9,xl-,pnzh=8,sxl=7,zllr=8,dbk-,cq-,qbvgb=6,ffl-,qd=2,crc-,jzd-,pdcbq=4,sfnjpz-,kghs-,mx-,gx=7,bzskg=6,kq-,ts=4,jktc=8,cds-,qsl-,tltpx=6,bm-,sdj-,ffcq=6,bgx=7,fcx-,ktkt=3,fqtqb-,bq-,lx=2,czb-,jq=2,bqrq=3,zl=5,tltpx-,plsvvv-,rf-,fmt=9,plqtt=5,mbdfxs-,fg-,pt=3,dk=6,qq=4,gzr=7,np=4,qg-,nsb=7,mb-,cbr=4,xnfm-,dv=8,vhp-,fpk=4,mh=2,bk-,ppb-,pgx=2,ffl=1,jh-,plqtt=7,frd-,tvdl-,rr-,tq=6,pfgnk=3,ksxh-,bgx-,bnmzn=3,bd=4,rnmb-,ckc=2,vrfdzt-,cj=9,nkl=2,rn=5,zst-,fjskf-,psfb-,bf-,kbll-,zst-,fmnc-,mscs-,vz-,xt=5,vf-,qg-,gl-,jjk-,bzr-,dcq-,zllr=5,zllr-,zt-,gnbct-,xzbn=8,ftkx=1,kbdtm=4,zbr=4,kzrm=6,sd-,pg-,crm-,kpnvd=5,qg-,qd=2,fxl-,vz-,dsv=5,drc=6,szm-,tfv=2,szl-,lbz-,tbdds-,zst=2,chx=8,cq=2,rt-,scm-,tmbl=5,kq=2,mpfq-,srqc-,nqth-,kkg=1,rx-,gf=3,pf=6,pp=5,tzv=3,dbp-,nv-,fv=1,gj-,bnt=1,xt=4,jks-,szx-,rfkc=9,pmnk=7,gzr-,ffcq-,tzv=9,jbd-,gjb=8,pjq=9,szx-,zvtxf-,zk=6,bs=6,frd-,gxg-,fst=5,tcndlm-,xvtc=9,rhqh=9,jdr-,js-,tgzc=7,tq=9,jff-,nktf=6,js=6,lmd=7,mztd=3,bbn=8,fxl-,tvdl=4,lgql-,zrb=3,jbd=7,vqc=6,kmx=3,dkn-,ms-,vx-,dsv=8,pjq=7,ft-,zrb-,pt=7,vfz-,jk-,mqsf=7,vqc=3,ffl=7,qrhm=1,hm=1,hpt=6,dr=9,dnl-,qrhm-,bpc=8,frd=6,qv=8,bcml-,bd-,tbdds=6,czb-,bk-,jb-,mh-,dcv=4,tzv=4,tpr=5,cpmg-,pmnk-,bsq-,mxzx-,mbs-,zq=8,fqzh=6,fjc-,mklk-,czch-,vrfdzt-,btf-,xq=5,czgv=9,ld-,bpc-,ntbv=7,cctp=1,gzr=6,bnmzn=2,bvv=6,bp-,pvvrr=6,qsl-,lx=5,ln-,svsqz-,jh=1,xgjmdq=7,fnhrz-,bql-,mvtk-,fv=1,jff=9,gnbct=2,nqth=6,vkq-,cpmg-,rs=3,lx-,jj-,pvvrr=9,kghs-,nr-,ld=7,vg-,ffl=9,lz=3,hs=5,qh=7,cjv-,dcq-,hp-,rmbp=3,jml-,crc=4,fjskf=6,rhqh=9,dql=4,knh-,cn-,vz=6,nt-,tgzc=9,fd=4,rbb-,fmt=9,lhnn-,gjb-,dx-,kq=9,bxls-,np-,rd=2,knd-,hxl=4,qrx=1,qtfk=8,gj=5,vvtr=1,ftb-,hrn-,gtjn=6,lmd-,fl=9,drh=3,nsb=8,vsh-,bsq-,bs-,ndbcd=4,plsvvv-,pm=8,gxp=5,nktf-,bbn=1,cn-,sv-,tcndlm-,fst=8,rmtvzc=6,hv-,bp-,rf-,zvkg-,jv=5,pvvrr-,qhxdp-,ftb=5,szl-,zldz-,ghnt-,knd=2,xgjmdq=9,vf-,vsh-,gj-,xvcc=4,zvtxf-,fqm-,qg=9,vvdq-,jvt=9,djz-,jcr-,fg-,hp-,vr=6,km-,hzm-,fk=1,crm=8,szl-,fdjc-,frd=9,phm=9,drh-,hm=8,jh=1,bk=1,jml-,fmnc-,dx=1,ckfx-,fk=8,qxx=5,pqvr=6,ntbv=6,jt=2,knh-,qhxdp=1,lh=4,ccxk=6,mdt=3,hzm=4,lh-,rb=1,bd-,fjskf=1,ssmj=6,fmt=3,jvt-,gb=7,nxq-,tgzc-,btf=8,xvcc=3,zpkg-,bs=2,ccxk-,mh=1,pjt=6,nzzm=6,zz-,rxnvs=3,bsq-,lsh=8,fsx=9,ddh=3,vqc-,glh=8,cvb=4,bql=8,lmd=8,kcj=4,ksxh=9,htckf=6,km=4,zbr=1,dv=7,cn-,glh=2,pb=8,tfv-,zl=5,kkg-,tjk-,gt=3,dnl=2,bv=8,jgq=5,dbk=5,bsnzng=7,vsg=3,lbz-,gxp-,jq=6,hz-,gcp-,qh=8,jmh-,dr-,rx=7,mbs-,rlzk=6,ljdk=5,jqh=7,qcpgc-,lgql-,bb=6,lgql-,jjk-,nv=4,vnnr=5,rhqh-,nsdzj-,xmv=8,xkg=6,zdh-,bv=4,nvf=3,js-,gmpn-,srqc=4,gpm-,sdj=9,jqh=9,fq-,gjb-,tht-,zldz=2,kr=6,psplc=6,mbs-,cn=8,kjj-,jqh=6,tlp-,bxls-,pmkn=4,jvt=8,tlp-,qg=8,np-,rnmb-,dms-,rx-,rhqh-,rbb=1,rvzvt-,nsdzj=8,mmc=2,xrrj=4,bf-,hkh-,kzrm-,vf-,nv=6,pqvr-,tbtnhh-,jgq-,qbk-,zk-,gx=4,lbz-,mqsf-,jj=6,rlzk-,tt-,rhrsrn=2,drh=4,bv=3,kgrz=2,qk-,rf-,dbk-,ktg=6,rxnvs=7,rvd=2,xf-,kc-,zrb-,ckc=9,xbk=4,lbkm=5,qtg=3,mdt=5,rbb-,cpk=5,fmnc-,ldv=9,jj=6,xkg-,ln=8,kghs=1,ndbcd-,kbdtm=1,xnfm=1,fjskf=2,mjrb-,jz=5,btf-,jml=7,vfz-,zbhd-,dsv=2,ftb-,ssvvgg=7,jk-,rpl=4,ksxh-,gvm-,zllr=4,dx=3,kln=8,fcx-,fk=5,cds=8,nm-,dr-,nsb-,ntbv=3,td-,mjrb-,rt=8,fjc=4,pf-,bbfl=5,fb-,rbzz-,lx-,dnl-,rmtvzc-,zbhvkq=6,nv=7,dtg-,nkl=4,phm-,bxls=7,tcxd=7,crc=3,qf-,lvg=9,gxp-,pvvrr-,dcq=1,ckz-,rbb-,qrhm-,nktf=3,gl-,xmv=7,df=7,pg-,ssvvgg=7,mbdfxs=9,gpxtnh-,jktc=6,qk=4,dr=2,ft-,lbz=4,mklk-,tzp-,hz-,nkl-,fsx=4,rqg=5,zq=7,vkq=3,ld=5,qxcn=6,ln=9,ffcq-,nxdfr=2,qcx-,sz=8,rd-,qcx-,rx-,qrx=4,ffcq=2,gcp=3,ktg-,zl-,tvdl-,hstk-,gl-,kbll=1,qrx-,fqm-,lsf-,pc=2,phm-,jktc-,mjrb=4,hkh=1,lb=6,kr=1,mztd=4,xlq-,df-,vkq=9,ldf=6,kgrz-,fjskf-,ktg-,rhrsrn-,lv-,qcpgc-,rb-,hlz=4,xkg=5,xvtc=2,ffcq-,rkzkmk=7,bbfl=1,slm-,jb-,ntbv-,bp-,vsh-,ppb=6,tcncr-,fbv-,qmd=1,rc-,lvg-,gfl=1,hj=8,jct-,hm=6,rmbp=7,rbb-,fn=7,kbdtm-,ssmj=6,np-,rx-,qb=2,rh=8,fq-,sqf-,drc-,tht-,xrvsd-,kvd-,vvp-,pg-,rmbp-,kr=7,lgql=3,hdm-,glh-,dbp=7,sd=4,rt=9,rhrsrn-,td=9,qhxdp=8,ch=4,rbzz=9,gjb=9,kl=9,pkps=1,dp-,glh-,kjj=6,kr=2,kgrz=4,cbr-,cd=1,ndz-,rd-,zldz-,xzbn-,dv=1,hs-,qqt-,ts=4,vx=7,mxzx-,qbvgb=1,fq=9,cn-,kvd-,fb-,zbhvkq=4,phm=9,slgcl-,qbk-,dz=7,rpjz=4,dhzd=5,cd=3,fsx=7,rnzc-,rc=6,clthb-,kgrz=7,rcl=8,tcxd-,gfl=6,hlbrz=3,dgkp-,qxcn-,tnm=1,jf-,zvkg=5,rc-,sxl-,slgcl-,nxq=6,mqsf-,pjq=8,xv-,pqvr=5,tlp=8,ckz=7,dgkp=2,hkh-,fxd=1,dr-,jktc-,dcv=6,cds=4,cvb-,gjvvl=3,slm-,tltpx=9,fmnc-,fcx-,fst=9,vkq-,qxx=1,rb=5,mkv-,svsqz=2,bs=9,szl-,jt-,vqc=1,szm=4,bp=2,rmbp=3,szx=7,rbzz-,jff=8,ldv-,xv-,rb-,zvh=4,fsx-,trq-,bnmbrm-,tcndlm=4,km-,mh=3,grzb=9,hrc-,gtjn=8,qdn-,trq=6,rkzkmk-,cn=8,szm=7,zmt-,vqc-,tcn-,mztd-,rcl-,rxnbf=3,hv=4,xq=9,vcgq=9,pmh=2,czch=5,dv=5,tq-,szl=5,bk=3,rr-,mbs=2,gcp-,fk-,kzdc-,vkq=4,qk-,hv=9,xvcc-,lbs-,bzskg-,sxl-,zj-,ssvvgg=7,kcj=8,bk-,rr-,zllr-,vcl=1,lhgp-,cq=1,xnfm-,dl-,rmtvzc=4,fk-,cpmg=1,szx-,dt=7,rvd=8,gtjn-,rr-,vx=6,srqc=2,czch=7,qv=8,dkn=5,ndz=8,bt-,szm-,mbs-,gpm-,sxl-,cj-,rx-,gzr-,gnbct-,nsdzj=5,sv=1,gzr-,pgx-,gjb=2,bnmzn=3,mh=6,ldf-,zk-,bcml-,rnmb-,dv-,bdl-,szl=9,lbs=3,crfqnr=5,pmnk=7,kpnvd-,nxdfr=1,bm=9,cn=6,zrb=9,jmh-,mklk=8,kln=1,vz-,vqc=5,ghtrm=8,pb=9,qh=6,zl=5,zllr=6,hpt-,dl-,ld=6,gnbct=6,vrfdzt-,bs-,qcpgc=9,lhgp-,lbkm=2,ckz=7,nq-,mb-,fsx-,sqf=4,cpmg-,rkzkmk-,fkc=1,mkv-,qbk=2,pjq-,nnn-,fxl-,lmd=1,gtjn=3,nfht=6,cn=2,dms=4,lsh=7,gl=7,hstk=2,jct-,dh=6,sj-,jbd-,fx=4,mqsf-,fdjc=8,rp=3,rkzkmk=3,rn=6,knh-,cpp-,tmbl-,jct=6,czch=9,fcx-,czb=7,bbfl-,qmmggl=1,mpfq=8,nb-,fst=6,sfnjpz=9,qdn-,nsdzj-,nt=9,bcml-,nfht-,kbll=5,gkk=7,dv-,rx-,jt-,hd=5,nzzm-,vnnr=5,lhnn=5,jf-,pjt=3,rvhdj-,rs=4,mdt-,rs=5,vcgq-,gt-,gf-,qjz=8,crfqnr=8,ghtrm-,fjskf=4,js=6,ghnt-,ccxk-,tlp-,rb-,fjc-,dx-,kqmxm-,hpt-,crc-,pnzh=9,jmh-,jqh-,fjc-,fbv-,lgql=8,dz=4,tzgj=6,rbb=3,nqth=6,fdjc-,jqh-,jvt-,bdl=7,xrvsd-,mztd=1,kmx=9,qrhm-,gnbct-,hg-,drc=7,fmnc-,ftb=1,ndbcd=6,szx=2,lmd=2,ssvvgg=1,cg-,fn-,smcl-,hrn-,qtg=8,cds=1,km-,czgv=7,hrn=1,mbs=2,knd-,ts=2,gtjn-,lb-,dv=1,ff=5,rqg=8,dms-,nsdzj-,bcml-,sz-,cg=8,rkzkmk-,jdr-,xb-,qjz=9,dhzd=7,hv=5,bd=6,drh-,xt=3,bfxtcj-,skb-,gf-,hbs=2,zqrvs-,dcq-,dttxng=6,lmd=9,crc=3,hrc-,zt-,rfkc=1,lbkm=5,dgkp=3,kl-,fqzh=4,fx=8,cctp-,ndbcd=9,pz-,tcncr-,rn-,rlzk=8,lg-,jbhf-,mqsf=7,cds-,kp=9,sj-,ndz=8,svsqz=4,fkc-,bbdf=4,jgz-,jktc=9,xs-,fqm=4,tht=1,cnsdxg=3,djz=6,pz=6,qs=2,lhnn=7,dt-,hg-,rmbp-,dtg=6,dl=1,kjj-,bm=1,ddh=4,fdjc=1,kn=5,mvtk-,tzp-,xgjmg=3,bqrq=3,fqzh-,psplc=2,fsx-,zmt-,svg=4,szl-,kln=4,njv-,djz=8,mjrb-,zj-,tfv-,mjrb=9,sqf-,mvkj=9,ssvvgg=1,qq=9,jktc=6,zbr=1,rtr=9,lmd=6,xbk=2,gfm-,kc-,bql=6,rxbhq-,cq-,cj-,rmtvzc=5,rbzz=6,gnbct=7,nvf=4,pjt-,gzr=5,vnrx=6,pjq=3,fsx-,nxq=4,tcndlm-,jml-,tlp-,dql-,bqrq=5,jcvp=5,sv-,dhzd=9,gpxtnh-,xt-,jbd-,lrm=5,vcmlg-,rnbjz-,zvkg-,kghs-,xmv=8,qk-,vkq-,tdl-,slgcl-,chx=2,drc-,xgjmdq-,nss=8,dgkp-,vnnr=9,bsq-,tmbl-,jh=5,lbkm-,stt=9,qs-,hj-,cpmg-,mscs=9,trssr=3,gb=2,fxl-,zt=6,pkps=6,vrfdzt-,bt-,sg-,rt-,xl=3,qg-,jjk=2,xvcc=9,ckz=5,fn=7,qf-,pqvr=5,zz=5,rxbhq-,rnzc=5,vvp=4,sfr=5,lg-,qv=3,hrc-,gzr-,bpc-,sg-,qs-,rd=9,cctp-,jz-,glh=6,fq-,fq=7,nzzm=3,pf-,mvtk-,ts-,rl=5,fq-,hxl=4,sf=3,hlbrz=8,zrb-,xt=2,brtzfc=9,ns-,rphhlq=4,rh=3,gxg=4,glh=1,tbtnhh=6,jz-,sqb-,gkg-,kzrm=7,bql-,gn-,hj-,ndbcd-,pp-,lsf=8,zqrvs-,zmt=6,ljdk-,cpj=8,mpfq=2,stt=8,dl-,hstk-,fst=5,tgzc=6,ljtf=4,rlzk-,mt=7,gxg=4,kkg=4,xs=7,cjt-,zst=5,tfv=5,hkh=3,ndz=1,dnl=2,ldf=1,jt-,hpt-,cj-,xb-,sfr-,mhlh-,fbv-,qb=8,plqtt=5,cvb-,stt-,mkv=7,nt-,qjz=9,cbr=8,jml-,zldz=9,dbk-,df=2,pdcbq=1,lgtr-,rpjz-,jj-,qfb-,vqc=7,dr=7,fbv-,cbr-,pmh=3,ljdk=3,sd-,hp=9,kzrm-,qd=6,lx=4,tbtnhh-,mscs-,szx=6,tzv=9,bvv-,hstk=9,ndz-,xbk-,bql=6,xhnpk-,kghs-,rm-,czb=6,pkps=8,dl=2,hd=6,dcv=9,hl=1,gvm-,zz-,xhnpk-,ff=6,jdr=7,xkg=3,blcnc-,fdjc=4,rxbhq=1,kl-,qcx=1,nb=2,zmt-,gbbq=7,cv-,stt-,lhnn-,bbn-,crfqnr=9,vhp=4,rxnvs-,dns=7,lrm-,ch-,psfb=5,qf=2,bqrq=2,fb-,fmnc=9,ffl=8,mdt=9,kc=3,rb-,jbd-,pb-,mscs=9,bf-,bzskg=3,tgzc=7,snvzv-,sdj=3,rm=5,fpkhl-,vx-,fmt=5,lbz=5,dp-,ft=2,bs=6,dx-,rcl=5,mztd=6,rvd=2,lrm-,plqtt=6,mqsf-,mvtk-,srqc-,gpm-,hg-,jf-,knh=2,xhnpk=4,gx-,ftb-,ssvvgg=9,gpm=8,knf=6,vvp-,jv-,bt-,qs-,lvg-,dgkp=8,ns=6,jct-,mxzx-,jz=8,dms=2,bq=8,ldv=3,psfb-,rtr=7,dtg-,szx-,rxbhq-,pvvrr-,cpj-,xrvsd=8,lbkm=5,dhzd=2,rtr=8,nm-,vsh=4,xzbn=2,drh-,mztd=7,xfd-,ckfx=3,snvzv-,lg=5,jz-,rpl=3,mpfq=9,gk-,ch-,mscs=8,ldv-,cpk=2,kqmxm-,xgjmdq=1,rn=7,vkq-,qf=1,zpkg=8,rxbhq=2,dt-,sd=2,pvb=1,htckf-,ffl-,knh=5,bfxtcj=3,fg-,tvdl-,nr=5,xs-,rbb=3,bq=7,cpmg-,dkn=6,xvtc-,dz=1,qsl-,kvd-,rt=1,rhrsrn-,grzb=3,svg=7,rnzc-,glh=7,cctp-,dcq-,cpp=4,rl=9,dgkp-,sg-,rhrsrn=8,svsqz-,tt=9,hxl-,jk=4,htckf=8,jks-,vvtr-,fpkhl-,hp-,qrx-,xvtc=7,vrfdzt-,bbdf=5,ffl-,rsx=1,gcp-,kqmxm=9,jqh-,xmv=3,gvm=1,mt-,bs-,tg=6,knh-,jb-,gc=2,hg-,fnr=5,mpfq-,sj=7,vkq=4,bbfl-,gmpn=4,pz=9,fn=1,hbl=3,ch=3,jktc=3,fmt=2,rnmb=1,hd-,vr=4,fmnc=8,plsvvv=8,cv-,ch-,rhqh=6,kn-,bbn-,nxdfr=6,hpt-,rl-,htfhlh-,plqtt-,nv-,dt=2,zbr=4,xrvsd=5,gc-,gxg-,rhrsrn=3,jz-,jkl=9,ckz-,dsv-,tpr-,jml-,fq-,ndbcd=2,pmkn-,bbk=3,jcvp=2,rf=3,dk=3,sfr=9,rphhlq=9,bvv-,fpk=4,kghs-,cqk=5,dttxng-,rb-,qd-,bxls=9,phm=6,qmd-,mklk=4,zk=9,qx-,drh-,hkh=6,mt=9,mt=3,tg=5,rx-,gl-,cd=5,sf-,rp=5,kghs-,df-,vnnr=8,vcgq=3,vfz=2,dz=3,hxl=2,xbk=7,drc-,mvkj=6,mmc-,jvt=1,lhnn=4,gk=7,kp=4,ndz=5,gkk-,cqk=9,ntbv-,crfqnr-,fpk-,tzgj=9,pmkn=1,rs-,bt-,mbls=1,nqth=6,fmnc-,gvm-,fn-,bbfl=3,tfv=8,xmv-,vg-,km-,jb-,zllr=8,rp-,cj=1,xfd=3,qsl-,lbs-,cn-,hd=7,stt-,jgq-,jqh=4,hlbrz=5,xv-,dttxng-,kc=6,pp=5,jvt=4,dql-,fxd-,ldf-,xj-,psfb=1,sfr-,fq-,sd-,clthb-,rnbjz-,pfgnk=8,jqh=5,hl=5,jff=9,qbk-,ms=8,rm=7,tcn-,cds=6,zqrvs=9,fd-,ghtrm=7,rh-,vcgq=4,jvt-,fqtqb=9,qxx=9,tpr=2,pvb-,mbls=7,mbdfxs=6,gtjn=3,cq-,rc=9,qb-,bsnzng=6,mscs=7,nxq-,gjvvl-,nr=7,rp-,rbb=5,fjc=9,gx=1,psfb-,hstk-,stt=8,kr-,qtg-,jgq-,dx-,fjc=2,xt=4,jjk-,jgq-,rxnbf-,lsh=2,vfz=3,gjj-,lb-,snvzv=9,bq=4,pjt-,slgcl=9,sdj=5,qg-,fjc=7,ljdk-,bbdf=6,fq=9,rvzvt-,bm-,fgp=5,smcl-,sfr=2,skb=5,snvzv-,tcxd-,bs-,rb=3,dkn=7,jcvp=7,mpfq-,fcx=2,fpkhl-,fdjc=2,nktf-,zllr=5,vr-,hp-,htfhlh-,blcnc=8,nqth=2,lvg=9,gpm=1,tcndlm-,szm=1,rt-,kbdtm=8,rnzc-,qc-,xnfm-,xq-,gpm-,dbk-,dx=3,fmnc-,mb-,vt=6,vcl=2,xb-,fg-,zdh=8,cctp=1,zllr=2,kkg-,bdl=9,jkk=2,bbdf=4,bvv-,jgz=1,knh-,jqh-,ppb-,kc=6,crfqnr=9,srqc-,psplc=9,hs-,mdt-,rpl=8,jmh-,cjv-,gb-,qcpgc=4,cpp=3,mmc=5,bqrq-,rcl-,rs=8,jt-,gzr-,fv=3,rn=7,zvtxf=8,zk=9,cctp=7,zqrvs-,zt=7,zrb-,kc=5,jf=8,ft-,kp-,mt-,cvb=8,rvd-,sfr=7,jzd=9,tltpx-,fbv-,rb-,tcncr-,hrn=3,hl-,mbs-,fk-,fjc-,ckz-,cctp=1,bqrq=7,fkc=6,jbhf-,fg-,bdl-,zrb=7,rsx=8,fg-,gpm=6,gc=7,bnmbrm=8,bbn=4,szl=2,knh-,tzp-,sj-,qd=5,sz-,pvb=4,lhgp=6,tvdl=3,fnr=5,rxbhq=4,kl-,nd-,kzrm=7,vvtr=3,kbdtm=2,tcndlm=5,fst=3,qk-,fxl=1,zvtxf-,htfhlh=2,qxx=8,kqmxm=7,rtr=3,ktg=5,pfgnk=3,mjrb-,vnrx=2,vvp-,vnrx-,knd=3,gb=8,hrn=2,gx=1,lb=1,gt-,cd=4,xs=9,rn=4,psfb-,sl-,phm=2,jcr-,mscs=9,lx-,fjc=3,cjv-,td=9,ljdk-,fdjc-,df-,nqth=3,bxls-,gmpn-,md-,dcv-,cs=8,jf=2,bbn=6,cnsdxg=4,plsvvv=3,jj=6,trssr-,qtfk=7,drc=2,rx=6,vvdq=8,xv-,cpp=3,cpk-,cs-,jgq-,drc=2,kbll-,smcl=1,cpk=9,rbb-,df-,tpr=7,mklk=2,qxx-,mdt=5,fgp-,szx=8,rhrsrn-,fjc=9,qrhm-,rnzc-,qqt-,vt-,fcgbsb-,cds=7,czb=5,df=5,bm=9,qsl=8,nv=8,vnnr=3,tbtnhh-,gxp-,psfb=2,ftkx-,brtzfc=7,qqt=2,mbs=2,gkg=5,qq-,crm-,sf=4,kc=7,nq-,jzd-,gzr-,mx-,pjq=9,bk=8,plqtt-,jgz=5,nxdfr=3,slgcl-,tmbl-,zvkg=6,pkps=8,ddh=1,bfxtcj-,dhzd=9,lgtr-,tzv=2,sg=2,hrn=5,bfxtcj-,ftb-,hbs=5,vnnr=7,vr-,nb-,qf=4,vdbg-,mscs-,sfnjpz=4,cbr-,lb=9,kzdc=4,mx=1,kghs-,ffcq=5,tvdl-,tmbl=4,vfz=7,pvvrr-,vfz=1,zqrvs-,vvtr=3,tzv=3,qh=3,ld-,ckz-,dv=7,svg=6,zt=7,hbs=1,cnsdxg=4,rt-,qf=9,lg-,nnn-,lhnn=7,fst=6,kzrm-,vvp=3,bm-,ppb=6,pjq=5,xgqsd-,dk=6,tbtnhh-,df=9,lrm=2,svsqz=7,ckc-,tzgj-,qk-,gx=5,gx=4,bzr-,fcx=2,mvkj-,rsx=9,gxg-,fjskf-,dcq=9,glh-,hl=9,zt-,kr-,zqrvs-,qv=2,vx-,fpkhl-,szx=7,mqsf-,kcj=8,xgjmdq=6,sz-,fcx-,zvh-,vdbg=8,sdj-,bb=2,bb=9,vnrx-,fpk-,gn=2,qv=2,fg=5,lhgp-,gnbct-,bcml-,ld-,tzgj=6,gt=1,dttxng-,pfgnk-,qg=1,nsdzj-,bql-,rf=9,qs=2,hbs=5,cnsdxg=6,mh=9,ft=3,plqtt=8,qc-,fqzh=8,bv=1,cg=2,xhnpk-,frd-,qc=6,kv=7,slgcl=9,slm-,lgtr=4,tzgj-,cnsdxg=6,vqc=2,ckc-,zz=2,htfhlh-,sfr-,nzzm-,md=9,sj-,mvtk=8,fbv=5,bql-,qfb=7,lx=7,xbk=5,fmt=1,xkg=4,jkk-,bb=1,plqtt=7,nf-,nxdfr=5,htfhlh-,gl=8,vvtr=8,gjj-,ftb-,xgjmdq-,cs-,rl-,ln-,vqc-,fpkhl-,rbzz-,rf-,gjb=6,lhnn=6,fcgbsb-,lbkm-,szx-,fcx=1,hstk=8,rm=7,psplc-,dkn=9,qh-,zt=8,vsg=4,mjrb-,qmd-,vhp=4,plsvvv=6,ppb-,gtjn-,fnhrz-,gn=4,rfkc=7,pvvrr=1,svg=5,lgql-,xzbn=1,qd-,rhqh-,zk=2,jf=9,fg-,rvhdj-,fst-,fkc-,mvkj=7,dcv=4,gjj-,dkn=1,hrn-,bb=9,ppb=7,dgkp-,zbhvkq-,md-,ckfx-,rlzk=7,fpk-,rs=7,gf=6,lx-,rd=8,fqtqb-,tfv=8,jzq-,qmd=4,vvp-,tgzc-,xgjmdq=8,lsf-,frd=1,nvf-,rxnvs=9,fkc-,rcl-,pmh-,lz=2,njv-,nfht=1,qv=9,kbdtm=1,cjv=8,lvg=8,xb=8,bd-,zmt-,dz=7,plsvvv-,slgcl-,gxg-,vhp=8,qdn-,jjk-,hz=9,bqrq=5,rxnbf-,qq-,zbr-,gbbq=9,mqsf=5,hg-,lb-,vcl-,gpxtnh-,qdn-,jf-,szx=7,mxzx=6,bq-,gnbct=3,fb=6,jt-,dsv-,bsq=3,bf-,xzbn=7,kdlss-,qcx=3,zbr=6,vcmlg-,skb=9,gb=7,lx=3,vsh-,plsvvv=4,mt-,dv=1,sthzq=6,xt-,zvtxf-,cjt=8,rn=2,ntbv=5,cq-,gvm-,rfkc-,pqvr-,zbr=9,kbll=6,sfr=4,qrx-,knh=4,md-,dkn=8,nzzm=2,dl=7,ksxh-,blcnc-,gf-,xb-,mbs-,tdl-,lvg=4,hlbrz=6,sg-,jz=5,djz=2,jb-,xb=3,mbls-,mhlh=2,tcncr=6,hbl=8,gc=1,tq=7,fcx-,mhlh-,rmtvzc=2,jzq=5,bbn=3,sfr-,slm=2,nfht-,vt=7,xq=9,qb=7,qxcn-,mjrb-,nqth=1,nsdzj-,lh-,cvb=8,hj-,bzr=2,fxd-,mjrb=2,qx=4,ckc=3,ft=5,sf=8,nb=4,hv-,kjj-,zllr=3,sl=3,bv=3,hpt-,bbk=2,ns-,bqrq=7,qxcn-,dbp=5,vcgq=6,lx=8,lbs=4,vnrx-,ftkx=9,bnt-,qhxdp-,vsg-,qs=1,np=9,ghnt=8,nxq-,jj=2,qx=1,zrb-,ln-,qf-,mb=5,fgp=3,ssvvgg=6,ndz=9,mbls=2,crfqnr=6,dhzd=3,rmtvzc=8,jff=3,zbhd=1,gx-,crm-,stt=7,bm-,hj-,cjt=1,plqtt-,fnr=9,gbbq=5,sd=2,jgz=4,vz-,jbhf=6,tt=3,hpt=5,td=1,pc-,nfht-,pjq=9,jkl=5,glh-,bnmzn=7,crc-,dcv-,hl=8,nxq=9,rc=3,fmnc=5,rxnvs-,rm=2,cvd-,pmnk=6,fjc-,ntbv=5,xt-,dnl=6,bsq=5,nkl=8,ljdk=6,jt=2,plqtt-,kp=9,qg=1,lb=5,hbs=1,qcx-,kzrm-,nktf-,bh=3,bq=8,qcpgc-,xnfm=7,pvvrr-,mkv=2,kghs-,dgkp-,nxq-,fjc=4,sfr-,fnhrz-,djz=3,ckz=2,dx-,jz-,qg-,tt=7,ddh=8,gl=1,jkl-,bdl=5,vvdq=2,ksxh-,bbn-,clthb=2,zpkg=1,hg-,dh=6,hbl=1,jt=5,gsr-,jgz=4,fv-,nm-,fjc=1,rxnvs-,zq=9,qxcn=4,ljtf=3,drh=6,np=9,dh=4,tvdl-,ssmj=8,gj-,rlzk=4,sz=2,hbs-,vcl=7,mvtk-,qhxdp-,bql=5,cbr=1,tzp=5,rnbjz-,xzbn=5,lvg-,nsdzj-,njv-,fmnc-,pfgnk=5,ghnt-,qxcn=4,nxdfr-,ktg=5,zbr-,bxls-,vkq-,pmh-,slm-,rqg=2,kln-,rr-,kzrm=9,cg-,dr=1,fsx=7,jbhf=7,fmt-,fmt-,rpl-,pqvr-,vntc=3,zbhd-,fl-,gjb=3,md-,kmx-,fg-,dz-,hrc=4,hm=6,tltpx-,xgjmg=6,rkzkmk-,kq-,hm=4,hj=2,kp=1,xrvsd=7,jbhf-,vkq=1,lsf=7,grzb-,bv=7,ndbcd=7,qd-,bqrq-,nt=3,zj=6,fqzh=5,slm-,fb=7,cpmg-,cj-,bcml=7,fk=1,jq=7,jz-,pmnk=3,vvdq=3,fnhrz=9,xfd-,srqc=6,jjk-,gpm=6,hl=1,dt=1,fx-,rh-,nv-,vntc=1,tdl-,td=2,fb=2,jjk-,ckz-,xt=9,qg=2,xt=5,ktkt-,cqk-,pkps=1,cn-,fqtqb-,hlz=3,mhlh=4,tbtnhh=2,bk-,cpk=5,mdt-,rc-,ljtf=2,mvtk=4,gpxtnh-,ln-,ljdk=2,czgv-,mb=1,xzbn=2,svg-,nm=4,zmt-,cqk-,lsf-,fjc-,dp=4,xbk=5,bf=7,qd-,jmh=3,lv=5,bnmzn=5,jj-,rbzz=2,xj=4,cpk-,kln=3,xf-,cj=2,rc-,cvd=3,qc-,jk=3,gbbq-,dkn=9,jj=8,qcx-,dttxng-,ns=2,fg-,zk=7,gnbct=3,clthb=2,sk-,lh-,sqf=4,hzm=3,fn=2,psplc=6,kghs-,rlzk=8,ckz=1,hlz=8,qbk-,nnn=9,ckz=3,hm=3,crc-,zllr=3,jjk=4,glh-,ff-,drc-,mh-,kpnvd=3,gtjn-,hv=4,hrn-,nv-,mklk=2,tg-,lb=9,vg-,hm-,gc-,nktf=7,nsb-,vnrx=9,zk-,xj-,fnhrz=9,kqmxm-,jdr-,bzr-,fmt-,cds-,bql-,cctp-,hzm=2,rbb=1,pgx=2,mscs=6,pvb-,drh-,krckj=9,nb=2,mjrb-,vpd=8,jk=3,hpt=5,sfr-,lbkm=5,dcv-,dk-,pnzh=3,zmt-,ssmj=9,chx-,xkg=4,lz=8,crfqnr=2,jcvp-,hl=9,hj-,kbdtm-,dgkp=8,gkk=7,ktg=6,fg=8,clthb-,lh=7,gb=3,cd-,dsv=9,plsvvv=4,hv=1,jgq-,bbdf-,bvv=9,ndbcd=7,rvhdj=1,mbls-,bh=2,hd=5,kdlss=3,xf-,dnl-,mmc-,hz-,kqmxm=5,dgkp-,dgkp-,xfd=9,hs-,xrrj=8,tmbl=9,lh=6,stt=4,czb-,rf=2,fq-,dns=2,scm-,sl=7,tcxd=3,fqzh-,vr=2,gk=2,cpk=6,pgx=5,ts=8,vvdq=9,jv-,xnfm-,hbs-,tmbl-,fcgbsb=3,vntc-,pt-,rr-,rn=7,qx=4,zvtxf=6,fst=4,lz=8,plsvvv=6,ljdk-,mqsf=2,rsx=6,bb-,mztd=3,jct-,jzd-,srqc-,hm-,xkg-,cv=8,qbvgb-,cpmg=1,rs=6,nfht=3,djz-,lh=1,nqtvnt=8,dns=4,jbd=6,btf=7,nkl=5,td=8,fgp-,ndbcd-,dsv=3,cnsdxg=4,jkl-,dx=4,sz=6,jkk=6,qxcn=2,mqsf-,nsdzj-,tltpx-,bb-,cq-,lsf-,zmt=6,chx-,vt=7,knf=2,gt-,fx=9,cds=3,vsg=8,tcncr-,dbp=7,bbk-,mx=1,sz=4,jjk=2,qs-,gcp=1,kdlss=6,pmkn=2,lbkm=1,rpl=3,pvvrr=9,hzm-,dtg=5,htfhlh=7,pg=8,md=5,dr-,bh=3,lbkm=3,kzrm-,rsx-,pm-,qg=7,nf=5,jf-,kbdtm-,bpc=1,xfd-,jcvp=9,rphhlq-,hkh=4,snvzv=4,rvzvt=4,jct=3,qq-,lmd=4,plsvvv=9,fgp-,kdlss-,gsr=2,gb=9,sz=2,fcgbsb=6,ch=1,pkps-,pz=3,qbvgb=5,hzm-,jk=5,dx-,mmc-,dkn=4,lb=3,fst=5,drc-,dz-,rvd-,jbd=4,qs=9,nqtvnt=3,fqm-,lz=2,qtfk=2,fg=5,vr=7,crfqnr-,dh=9,kmx-,sk-,cnsdxg=6,rqg=9,mbdfxs=2,qg-,krckj-,jt-,fdjc=1,vvp=7,kn=6,rc=6,czgv-,nb=7,fsx-,bs=7,cg-,lbz=3,cvb-,gxg=9,vcgq=8,tv-,ghnt=1,gpxtnh-,zpkg=2,pqvr-,qs-,hj-,gn-,clthb-,fjc-,vntc=5,tjk-,vkq=8,vcgq-,tlp-,jbhf-,zvtxf=4,pc-,dms=8,nqth=2,pkps-,cpk=2,czb-,rm=5,hbl=8,hj=1,lbs-,tlp=9,kjj=5,brtzfc=1,hz=8,jcr=3,chx-,bzskg=8,fcx-,fjgr-,jq=9,nf-,gjvvl=1,tcxd=7,bcml=4,glh=3,szx=1,vkq-,xlq-,mztd=9,cvd=2,xj-,qd-,ktg-,nd=5,mscs-,vntc=7,rl=8,dkn-,lrm=4,pmh=4,ljtf=2,ljtf-,cjv=4,lbz=3,gpxtnh=1,hm=1,jmh-,xq=5,ccxk-,qrhm-,tht=7,nf=7,fl=4,jzd=4,hlbrz=1,gt-,tjk=6,nvf-,qrhm-,ghtrm-,gzr-,tvdl=6,gmpn=9,rm-,hv-,gfm-,mscs=7,ktkt=5,cjv=6,zbr=6,gpm-,fv-,rphhlq=9,mdt=8,pgx-,lvg=8,gt=6,bbk=8,brtzfc=3,kcj=1,qq-,pnzh-,mscs=4,fpk-,df=6,hlz-,sf=2,tcxd=5,hrc=4,td=7,zq=7,jq-,hd-,vg=7,rnzc=3,xrrj=7,vr-,dp-,zbhvkq-,qq-,gjvvl=4,xb=2,ndbcd-,smcl=6,zllr-,sg-,dcv=3,rsx=8,fqtqb=7,gk-,nxdfr=5,gtjn-,slm-,tlp=6,hs=2,gbbq=7,xv=8,qtg-,ln-,mkv-,lz-,brtzfc-,gjj=8,gfm=4,kvd-,hrn=3,slm-,nsdzj=5,tcn=8,np-,pdcbq=5,fd-,bbdf-,qbk=7,sqb=6,gzr=8,jb-,gzr=4,fpk-,lv-,cn-,vmh=4,kp-,lv-,jzq=4,ts-,kjj-,vcmlg=5,md-,zk-,lh=2,ddh-,bm-,sqf-,tht=1,mbdfxs=7,mjrb=6,bnmbrm=4,pg=1,vqc-,zl-,bh-,js-,vcl=9,pkps=8,pvvrr=1,qc=3,rs-,ksxh=4,fjskf=4,tcxd=3,smcl-,qfb=2,psplc=9,nb=4,qd=2,gj=1,jt=4,xrvsd-,rbzz-,bb=8,cn-,jh=4,qc=4,rvhdj-,kbll=6,qk-,jf=6,tfv-,np-,vvtr-,rvzvt=5,cd=5,cs-,nfht-,gcp-,nv=4,dnl=1,ch-,pc-,np=3,rc=7,kghs=6,knd-,pp=5,ldf-,sz=9,fxd-,dh-,jjk=4,brtzfc=2,jgz=4,bzskg-,sv=9,nv-,zpkg-,cqk-,nktf=5,fg-,kbll=1,ntbv-,hbl-,tt-,zldz-,nxdfr=8,xb-,qqt-,mmc=5,zpkg-,dbp-,mh-,jgz-,hl=6,dr-,dp-,lmd=8,jktc=8,gk-,nr=5,xhnpk=4,glh-,mt-,tzv=5,qxx-,jkk-,bnmzn-,zvtxf-,szm=1,rh-,cnsdxg-,gpxtnh=9,fst=1,zbhd-,vqc-,tmbl=2,tltpx=6,dcq-,rpjz-,scm=9,pjq=6,lh-,mmc-,gjb=1,cpk=3,rp=6,jcr-,rn-,sj-,pc=5,ghnt-,pqvr=3,jjk-,ndbcd=5,jmh-,zvkg=1,plsvvv-,dk=6,dcq=8,fgp-,rkzkmk-,qbk=7,rmtvzc=8,zpkg-,bbk-,rmbp-,ppb=1,zpkg=7,gfl-,ms-,bm-,vdbg-,jv=3,vcmlg-,plqtt-,hz-,gj=6,kzdc=8,xvtc-,zldz=5,fkc-,zpkg=5,qxx=4,nss-,pc=6,szm-,tcn=9,fk=3,zl=7,tzp-,lmd=1,vvdq=9,jvt=4,bbfl-,kr=5,fxl-,lh=4,ssmj=6,dttxng=3,gmpn=4,fb-,ssmj=9,bt-,fb=3,pc=5,knh-,xf=3,mvkj-,czch=4,dx=4,jmh=3,ddh-,xmv=1,nt=7,gnbct-,cpj=1,gkk=7,rvzvt-,ppb-,zj=3,cnsdxg=6,ns=3,mmc-,cnsdxg-,ndz=8,dcq=1,lz-,vpd=5,czgv-,vf-,mxzx=5,bq-,lbz-,szx-,pnzh=6,xbk=4,vsg=3,ft-,ghnt=7,dql-,mt=7,qmmggl=4,dttxng=2,kgrz=1,vcl-,jgz=7,rqg-,fqm=8,nqtvnt-,dkn=6,bbk=2,qb=4,gfl-,rvzvt=8,gpxtnh=4,vfz=4,zm=8,sxl=4,sthzq-,rvhdj=5,jdr-,nf-,kr=9,rp=1,pm=1,szl-,mpfq-,ssvvgg=7,bvv-,vcl=6,ckfx=8,pb=7,sqb-,kln-,kpnvd-,qc-,mkv=6,mbdfxs=1,rlzk=5,qh=9,ssmj=4,ldv-,hs-,clthb=4,sl=2,vg-,hrc=3,kl-,gfm=5,gcp=3,rcl-,nxdfr=1,ffl=2,ccxk=8,bk-,ghtrm=2,tq-,pnzh-,nm-,rm-,pnzh-,knf=7,cqk-,pb=9,qdn=3,qf=2,jzq=6,smcl=3,rnbjz-,lmd=9,ljtf=6,jbd-,bzr-,mb-,tcxd=1,bp-,scm=9,nd=3,snvzv-,kn-,bb=3,fd=8,gkk=7,rfkc=9,rm=2,cg-,kdlss-,jcr-,fkc-,pmnk-,gnbct=7,zq-,slgcl=6,psplc=4,lbs=6,dms-,bk=6,szl-,jdr-,rvhdj=4,nkl=3,vpd=5,fb=5,fgp-,jj-,kzdc-,xj=2,bgx=5,tmbl-,kghs-,sdj-,hg=5,sg-,pgx-,vz=4,fnr-,kcj=6,pmkn-,pg=7,xbk-,lv-,hlbrz-,fv-,cjv=9,fqzh-,jcvp-,rvzvt-,bbk-,vf-,rp=8,lhnn=2,fdjc=3,qfb=2,jmh=6,rx-,qx=3,sz=1,rt=6,rf=9,lh=6,tcndlm-,ldv=2,jb=2,gx=4,jff-,gx=5,ch=3,ms=7,jcr-,nsb=5,crfqnr-,pqvr=3,nxdfr-,cn=7,xlq=9,tcxd=4,nktf=3,fpkhl=6,pmkn-,qqt-,kghs=8,lhnn-,ff-,bsq=7,vfz=1,tt=1,psplc=3,sdj=6,cbr-,rs=4,rp-,vg-,rc-,vr-,cpk-,bq-,bbfl-,sdj-,hzm=1,xkg-,rc-,bsq-,hd-,ckc=5,slm=4,bq=2,qf=3,sl-,rm-,dcq=2,xgjmdq-,sz=2,gpxtnh=7,czch=5,kl=4,jktc-,zllr=3,pb-,xt-,mpfq=8,bbdf-,bpc-,tvdl=2,trq-,zbhd=9,bbdf=2,rpl=3,psplc-,pg=3,lb-,md=2,rcl-,vpd-,psplc-,ljdk-,nb=1,ckfx-,xq=5,zj-,krckj=9,mztd=7,nsb=1,qqt-,mbdfxs-,mhlh-,sg-,qtg=4,xhnpk-,lbkm-,mh-,gl-,zt=4,srqc=1,jj=8,rn-,rd-,vt-,dql-,ssmj=7,pt=4,vnnr-,rbb-,zbhd=7,zpkg=4,hkh-,mjrb-,fcx-,zvtxf-,hrc=8,tv=4,ktg=6,pmnk=8,dttxng=2,mx-,rt-,mhlh=5,kcj=5,nvf=2,vvp-,rnmb=6,slm-,pjq-,vg=7,pc=6,bqrq-,xgqsd=7,bv-,xb-,ndbcd=8,qcpgc-,nsb-,kbll-,knd-,pjt-,qd-,jf=8,hj-,fg-,gb-,bbdf=2,rpl=1,mvtk-,zldz=5,ldf=9,ftkx-,ch-,zm-,fd-,jml-,rb-,dnl=2,vcmlg=6,vntc=6,bcml=2,tnm=3,xrrj=7,gn-,cpmg-,hg=7,ssvvgg-,mbdfxs-,zbhvkq-,rc-,plqtt=6,bb=7,mbs=5,zq=3")

(defn hash-letter
  [current-value letter]
  (mod (* 17 (+ current-value (int letter))) 256)
  )

(defn hash-word
  [word]
  (reduce hash-letter 0 word)
  )

(defn part1
  [line]
  (let [words (str/split line #",")
        hashes (map hash-word words)]
    (apply + hashes)
    )
  )

(defn make-lens
  [label n] {:label label, :count n})

(defn make-boxes
  [] (map (fn [id] {:id id, :lens []}) (range 256)))

(defn group-by-id
  [boxes]
  (reduce-kv (fn [m k v] (assoc m k (first v))) {} (group-by :id boxes)))

(defn with-lens-only
  [boxes-by-id]
  (let [boxes (filter #(pos? (count (:lens %))) (vals boxes-by-id))]
    (group-by-id boxes)
    )
  )

(defn drop-len
  [box label]
  (let [lens (:lens box)
        lens (remove #(= label (:label %)) lens)
        box (assoc box :lens (vec lens))]
    box)
  )

(defn put-len
  [box label count]
  (let [lens (:lens box)
        lens (mapv #(if (= label (:label %)) (assoc % :count count) %) lens)
        lens (if (every? #(not= label (:label %)) lens)
               (conj lens (make-lens label count))
               lens
               )
        box (assoc box :lens lens)]
    box
    ))

(defn execute
  [command boxes-by-id]
  (let [[_ label n] (re-matches #"(\w+)[-|=](\d*)" command)
        box-id (hash-word label)
        box (get boxes-by-id box-id)]
    (cond
      (str/includes? command "=") (assoc boxes-by-id (:id box) (put-len box label (read-string n)))
      (str/includes? command "-") (assoc boxes-by-id (:id box) (drop-len box label))
      )
    ))

(defn focusing-power
  [boxes-by-id]
  (let [boxes (vals (with-lens-only boxes-by-id))]
    (loop [boxes boxes
           power 0]
      (if (empty? boxes)
        power
        (let [box (first boxes)
              box-id (:id box)
              lens (:lens box)
              lens-power (map-indexed (fn [idx len] (* (inc box-id) (inc idx) (:count len))) lens)
              total-lens-power (apply + lens-power)]
          (recur (rest boxes) (+ power total-lens-power))
          )
        )
      )
    )
  )

(defn part2
  [input]
  (let [boxes (make-boxes)
        boxes-by-id (group-by-id boxes)
        init-seq (str/split input #",")
        boxes-by-id (reduce (fn [boxes-by-id command] (execute command boxes-by-id)) boxes-by-id init-seq)]
    (focusing-power boxes-by-id)
    )
  )


(deftest day15-test
  (let [boxes (make-boxes)
        boxes-by-id (reduce-kv (fn [m k v] (assoc m k (first v))) {} (group-by :id boxes))]
    (testing "day15"
      (is (= (hash-letter 0 \H) 200))
      (is (= (hash-letter 200 \A) 153))
      (is (= (hash-word "HASH") 52))
      (is (= (part1 example-input) 1320))
      (is (= (part1 puzzle-input) 512950))
      (is (= (get (execute "rn=1" boxes-by-id) 0) {:id 0 :lens [{:count 1 :label "rn"}]}))
      (is (= (get (->> boxes-by-id
                       (execute "rn=1")
                       (execute "cm-")
                       ) 0) {:id 0 :lens [{:count 1 :label "rn"}]}))
      (is (= (with-lens-only (->> boxes-by-id
                                  (execute "rn=1")
                                  (execute "cm-")
                                  (execute "qp=3")
                                  )) {0 {:id 0 :lens [{:count 1
                                                       :label "rn"}]}
                                      1 {:id 1 :lens [{:count 3
                                                       :label "qp"}]}}))
      (is (= (part2 example-input) 145))
      (is (= (part2 puzzle-input) 247153))
      )))
