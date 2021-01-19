#a
def simetricen(niz):
    return niz == niz[::-1]
#b
def stevilo_delov(niz):
    def trenutno(niz,i):
        if simetricen(niz):
            return 1
        else:
            if simetricen(niz[:i]):
                return 1 + trenutno(niz[i:],-1)
            else:
                i-=1
                return trenutno(niz,i)
    return min(trenutno(niz,-1), trenutno(niz[::-1],-1))

#d
def naredi_sez(niz):
    if len(niz) ==0:
        return []
    else:
        return [niz[0]] + (naredi_sez(niz[1:]))

def vsotno_simetricen(niz):
    dolzina = len(niz)
    sez = naredi_sez(niz)
    n_2 = dolzina//2
    leva = 0
    desna = 0
    for i in sez[:n_2]:
        leva += int(i)
    for i in (sez[::-1])[:n_2]:
        desna += int(i)
    return leva == desna

#e
