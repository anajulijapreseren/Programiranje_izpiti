
def f(k,n):
    """funkcija vrne st zaporedij naravnih stevil vklucno z 0,dolzine n,
    ki se zacnejo z 0 in je abs razlika mad zaporednima členoma <= k"""
    if k<0:
        return 0
    else:
        if n<0:
            return 0
        elif n == 0:
            return 1
        elif n==1:
            return 1
        else:
            return (k+1) * f(k, n-1)


print(f(0,3000))