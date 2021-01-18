#3.NALOGA
#ZABOJNIKI:1,3,4,7,10
#LADJA:5,40,300

zabojniki = [1,3,4,7,10]

def natovori(nosilnost, zabojniki):
    možnosti = 0
    if nosilnost < 0:
        return 0
    elif nosilnost == 0:
        možnosti += 1
        return možnosti
    else:
        for i in zabojniki:
            možnosti += natovori(nosilnost-i, zabojniki)
            zabojniki = zabojniki[1:]
        return možnosti


print(natovori(5, zabojniki))

    