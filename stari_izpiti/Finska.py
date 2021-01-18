test = [
    [(1, 10), (3, -10)],    # 0
    [(2, 10), (5, -20)],    # 1
    [(3, -10)],   # 2
    [(4, 15)],  # 3
    [(5, 0)]]   # 4

def pobeg(mesta):
    konec = len(mesta)
    def trenutno(indeks,denar,pot):
        if indeks >= konec and denar>=0:
            pot.append(indeks)
            return pot
        elif indeks >= konec:
            pot.remove

