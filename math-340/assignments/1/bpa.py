import json

def bpa(boys, girls):
    assert len(boys) == len(girls)

    engagements = {} # map from boys to girls

    while len(engagements) < len(boys):
        b, b_prefs = [(i, b) for i, b in enumerate(boys) if i not in engagements][0]
        for g in b_prefs:
            print(b + 1, 'proposes to', g + 1)
            g_prefs = girls[g]
            got_engaged = False
            for fiance in (b_ for b_, g_ in engagements.items() if g == g_):
                print('\tbut she is engaged to', fiance + 1)
                if g_prefs.index(b) < g_prefs.index(fiance):
                    print('\tbut she prefers', b + 1, 'to', fiance + 1)
                    del engagements[fiance]
                    engagements[b] = g
                    print('\tso she breaks up with', fiance + 1, 'and dates', b + 1)
                    got_engaged = True
                else:
                    print('\tand she prefers', fiance + 1, 'over', b + 1)
                    print('\tso she remains with', fiance + 1)
                break
            else:
                print('\tshe is not engaged so she accepts')
                engagements[b] = g
                got_engaged = True

            if got_engaged:
                break

    return engagements

def assignment1q1():
    p = lambda x: [y - 1 for y in x]

    BOYS = [
        p([3,2,1,4,5]),
        p([2,1,3,5,4]),
        p([2,5,4,3,1]),
        p([1,3,4,2,5]),
        p([2,3,1,5,4]),
    ]
    GIRLS = [
        p([5,2,1,4,3]),
        p([3,1,4,2,5]),
        p([2,5,4,3,1]),
        p([1,3,4,5,2]),
        p([4,1,5,3,2]),
    ]

    matching = bpa(
        boys=BOYS,
        girls=GIRLS,
    )

    matching2 = bpa(
        boys=GIRLS,
        girls=BOYS,
    )

    print(json.dumps(matching, indent=2))
    print(json.dumps(flip(matching2), indent=2))

def flip(m):
    m_ = {}
    for k, v in m.items():
        m_[v] = k
    return m_
