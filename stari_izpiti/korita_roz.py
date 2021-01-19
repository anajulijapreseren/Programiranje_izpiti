# n= sirina balkona
# m= stevilo korit
# l = sirina korita

    
def korita(n,m,l):
    def trenutno(mesto,st_preostalih_korit):
        if (st_preostalih_korit == 0 and mesto <= (n+1)):#ta = in +1 dam ker moram vedno ko postavim preskocit eno mesto in "pri zadnjem lahko padem ve"
            return 1
        elif mesto >= n:
            return 0
        else:
            postavi = trenutno(mesto + l + 1, st_preostalih_korit-1)
            preskoci = trenutno(mesto+1, st_preostalih_korit)
            return (postavi + preskoci)
    return trenutno(0,m)


def razlicna_korita(n,sez):
    def trenutno(mesto,i):
        if (i==len(sez) and mesto <= (n+1)):#ta = in +1 dam ker moram vedno ko postavim preskocit eno mesto in "pri zadnjem lahko padem ve"
            return 1
        elif mesto >= n:
            return 0
        else:
            postavi = trenutno(mesto + sez[i] + 1, i+1)
            preskoci = trenutno(mesto+1, i)
            return (postavi + preskoci)
    return trenutno(0,0)
