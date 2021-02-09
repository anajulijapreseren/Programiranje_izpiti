import time

# =============================================================================
# Po koncu karantene načrtuje Rožle planinski pohod po svojem najljubšem
# gorovju. Zamislil si je že pot, ki ima veliko priložnosti za fotografiranje.
# Ker pa uporablja zastarel telefon, ima na pomnilniku prostora za zgolj dve
# fotografiji. Da bi ti dve sliki čim bolj izkoristil, želi da je med lokacijo
# prve fotografije in lokacijo druge fotografije kar se da velik vzpon.
#
# Kot vhod dobimo seznam nadmorskih višin za vse razgledne točke v takšnem
# vrstnem redu, kot si sledijo po poti. Na primer:
#
#    [350, 230, 370, 920, 620, 80, 520, 780, 630]
#
# V zgornjem primeru se Rožletu najbolj splača slikati na točki 5 (višina 80m)
# in nato na točki 7 (višina 780m), saj se je med njima vzpel za 700 metrov.
# Čeprav je med točko 3 (višina 920m) in točko 5 (višina 80m) večja višinska
# razlika, se je med točkama spuščal in ne vzpenjal, zato ne prideta v poštev.
# =============================================================================

# (a)
# -----------------------------------------------------------------------------
# Napišite funkcijo, ki v času `O(n log(n))` ali hitreje izračuna največjo
# višinsko razliko med optimalno izbranima točkama. Časovno zahtevnost
# utemeljite v komentarju.
# -----------------------------------------------------------------------------
def max_razlika(l):
    def trenutno(i, minimum, maximum,sez):
        if i == len(sez):
            return maximum - minimum
        elif (sez[i] > maximum):
            return trenutno(i+1, minimum, sez[i], sez)
        else:
            return trenutno(i+1, minimum, maximum, sez)
    return max([trenutno(0, (l[j:])[0], (l[j:])[0], l[j:]) for j in range (0,len(l)-1)])

#max([trenutno(0, l[0], l[0], l[j:]) for j in range (0,len(l)-1)])




#IDEJA in UTEMELJITEV
#a)najprej se sprehodimo čez cel seznam in ugotovimo max razliko če slikamo na prvem in nekje drugje
#b)potem "odstranimo" prvi element in ponovimo točko a)
#c)na koncu vzamemo max od vseh teh razlik


#ce je dolzina seznama n, bomo morali postopek ponoviti za n seznamov dolzin od n do 1
#ce ima podseznam m elementov, bomo pogledali vsakega izmed njih
#torej porabimo cas: 
# - razdelimo na n podseznamov:O(n)
# - se sprehodimo čev vsak podseznam :O(n) + O(n-1)+ ... +O(1) 
# - cas da vzamemo maximum iz seznama z n elementi(vgrajena funkcija max )



# (b)
# -----------------------------------------------------------------------------
# Prejšnjo rešitev prilagodite tako, da vrne zgolj indeksa teh dveh točk. Pri
# tem poskrbite, da ne pokvarite časovne zahtevnosti v `O` notaciji.
# -----------------------------------------------------------------------------
def max_razlika2(l):
    def trenutno(i, i_min, i_max,minimum, maximum,sez):
        if i == len(sez):
            return [maximum-minimum,(i_min, i_min + i_max)]
        elif (sez[i] > maximum):
            return trenutno(i+1, i_min, i,  minimum, sez[i], sez)
        else:
            return trenutno(i+1,i_min, i_max, minimum, maximum, sez)
    return max([trenutno(0,j,j, (l[j:])[0], (l[j:])[0], l[j:]) for j in range (0,len(l)-1)])[1]

#print(max_razlika2([350, 230, 370, 920, 620, 80, 520, 780, 630])) 

#funkcija dela isto, le da si še zapomne mesto min in max ideksa
# (mesto max je gledano glede na min, zato na koncu se pristejemo min )
# koncamo tako da pogledamo max razlik in vrnemo ustrezna indeksa



def rozle_vrednost_deli_in_vladaj(xs):
    # O(n logn)

    def rozle(i, j):
        if i >= j-1:                                 
            return 0

        half = (i+j) // 2 
        both_left = rozle(i, half)
        both_right = rozle(half, j)
        different = max(xs[half:]) - min(xs[:half])

        return max(both_left, both_right, different)

    return rozle(0, len(xs))

def rozle_vrednost_pametno(xs):
    # O(n)

    cummin = []
    opt_min = xs[0]
    for x in xs:
        if x < opt_min:
            opt_min = x
        cummin.append(opt_min)

    print(cummin)

    return max([(x - opt_min) for (x, opt_min) in zip(xs, cummin)])
