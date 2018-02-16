import itertools
import math
from matplotlib import pyplot as plt
import statistics

# Make a dictionary: keys = coordinates, values = 1(good) or 0(bad)
dt1 = {(1,4):0, (1,5):1, (1,6):0,(2,3):0, (2,4):0, (2,5):1 \
, (2,6):1, (2,7):0, (3,2):0, (3,3):0, (3,4):0, (3,5):1 \
, (3,6):1, (3,7):1, (3,8):0, (4,1):0, (4,2):0, (4,3):0, (4,4):0 \
, (4,5):1, (4,6):1, (4,7):1, (4,8):1, (5,1):0, (5,2):1, (5,3):1 \
, (5,4):0, (5,5):0, (5,6):0, (5,7):0, (5,8):0, (6,1):0, (6,2):1 \
, (6,3):1, (6,4):1, (6,5):0, (6,6):0, (6,7):0, (6,8):0, (7,2):1 \
, (7,3):1, (7,4):1, (7,5):1, (7,6):0, (7,7):0, (7,8):0, (8,3):0 \
, (8,4):0, (8,5):1, (8,6):0, (8,7):0, (9,4):0, (9,5):0, (9,6):0}

# A function that would either plot a variogram or return the values
def variogram(dic, plot=True):
    # make pairs of two key-value pairs from the given dictionary
    key_comb = list(itertools.combinations(dic.items(), 2))

    # make an empty dictionary to store the preliminary euclidean distances as keys
    # and the squared differences as values.
    new_dict = dict()
    for comb in key_comb:
        # this is a euclidean distance of two points
        edis = math.sqrt((comb[0][0][0] - comb[1][0][0])**2 + (comb[0][0][1] - comb[1][0][1])**2)
        diff_squared = [(comb[0][1] - comb[1][1])**2]
        if edis in new_dict.keys():
            new_dict[edis].append(diff_squared[0])
        else:
            new_dict[edis] = diff_squared

    # make another dictionary to store values in designated bins.
    # merging items of non-integer keys with those of integer keys
    another_dict = dict()
    for key in list(new_dict.keys()):
        if math.floor(key) in another_dict.keys():
            another_dict[math.floor(key)].extend(new_dict[key])
        else:
            another_dict[math.floor(key)] = new_dict[key]

    # for each bin, average the values
    new_dict_averaged = dict((k, statistics.mean(v)) for k, v in another_dict.items())

    # just print a table of the distances and the corresponding average diff squared, if plot == False
    if not plot:
        table = '\n'.join(['{}\t\t{}'.format(k, new_dict_averaged.get(k)) for k in sorted(new_dict_averaged)])
        print('distance\tAve diff ** 2\n'+table)
    else:
        plt.plot(list(new_dict_averaged.keys()), list(new_dict_averaged.values()), '*')
        plt.title('A variogram of Example 6.4', fontsize = 18)
        plt.xlabel('Distance', fontsize=16)
        plt.ylabel('Ave diff ** 2', fontsize=16)
        plt.show()

variogram(dt1)
