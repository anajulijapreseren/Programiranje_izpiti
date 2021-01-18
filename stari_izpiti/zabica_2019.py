
mocvara = [10]*50


from functools import lru_cache

def frog_escape(swamp):
    @lru_cache(maxsize=None)
    def escape(k, e):
        if k >= len(swamp):
            return 0
        else:
            e += swamp[k]
            return 1 + min([escape(k + d, e - d) for d in range(1, e + 1)])
    return escape(0, 0)

