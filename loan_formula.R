#
#Formula from https://www.mtgprofessor.com/formulas.htm
#
#fixed monthly payment (P) required to fully amortize a loan of L dollars over a term of n months at a monthly interest rate of r
Payf=function(L,n,r){
    c1=r/12
    L*(c1*(1+c1)^n)/((1+c1)^n-1)
}
#remaining loan balance of a fixed payment loan after m months
Balf=function(L,n,r,m){
    if(m==0) return(L)
    if(m>n) return(0)
    c1=r/12
    L*((1+c1)^n-(1+c1)^m)/((1+c1)^n-1)
}
#interest paid at the i-th month of a fixed payment loan
Intf=function(L,n,r,i){
    c1=r/12
    Balf(L,n,r,i-1)*c1
}
