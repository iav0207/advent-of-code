Spec https://adventofcode.com/2021/day/22

Rebooting a submarine reactor.

Calculating immense volumes of enormous cuboids overlapping with each other.

Learned

1. coordinate compression
2. multiprocessing in Python and sharing numpy arrays fairly between the workers
5. there is no pypy3 for arm64 architecture (M1 chip)
3. that 3 dimensions is too many for me
4. off-by-one errors (oh I've had them enough) turn into off by over 9000 all over the place

Execution

```bash
./advent_22.py < input.txt    # to run on one cpu core
./advent_22.py 6 < input.txt  # to utilize up to 6 cores
```

Output

```
[[1414 2285 3535 ...  334 4159    1]
 [ 856   19  921 ... 3203    1    0]
 [ 531  616 1867 ...    0    0    0]]
Amount of work: 569027250 3d elements.
Sharing the job: spawning 8 workers
NEW WORKER: shape (104, 830, 825) start i =     0. To do:   71214000
NEW WORKER: shape (104, 830, 825) start i =   104. To do:   71214000
NEW WORKER: shape (104, 830, 825) start i =   208. To do:   71214000
NEW WORKER: shape (104, 830, 825) start i =   312. To do:   71214000
NEW WORKER: shape (104, 830, 825) start i =   416. To do:   71214000
NEW WORKER: shape (104, 830, 825) start i =   520. To do:   71214000
NEW WORKER: shape (104, 830, 825) start i =   624. To do:   71214000
NEW WORKER: shape (103, 830, 825) start i =   728. To do:   70529250
Done 14.04%
Done 28.08%
Done 42.13%
Done 56.17%
Done 70.21%
Done 84.25%
Done 98.30%
1387966280636636 cubes are lit.
```

Or, on the example input:

```
[[ 8934   280  9800  2589   341  2334  2289  3720  5472  1326  5876  4457
   2313  5217  3457   910  2968    22  1966  2174   185   718  5264  3217
   3275  3186  3147   244  1450   791  2196  2132   485  6035  2699   446
   1583    55    69  1308   557  2267  1200  1686    22  3281   345   854
    683  3014   201   334   677     5     1     2     8     1    12     6
      9     5     6    10    10     9     2     3     8  1984  6444  2031
   1087   498  1436   460   793  1602  1553    84   229  3043   982  2701
    724  5300  4917   815   783    20  2950   591  2595  2726  3062   628
   5608   226   372  1794  4665  7020   348  9170  1469  3225 13969   741
    684  2178  3798  1784  1550  2540  2791  6858     1     0     0]
 [ 4332 11759  9071   710  8699  8656  5818  3360   446   701  5712  1732
    940  6494  2671  2661   851   502   519  2339   730  1462  1033  1299
   8593  2807  2298   994  6236   257   456   440   361   830   852  1047
    727  6654   859   311   453   805  5747  1295     1     4     1     7
      1     1     4    16     5    13    13     2     1    17     3     2
      6  2378   260   311   413  1202   210  4272  2506   780   842  2289
    555  1578   512  4330   159  1690  1406  1730  1059  1031  1445  1598
   3401  4414   556    14  2180  3787  1982   380   658   118  4069  5753
    841  1974  4810  1228  2056  1203  6666  4939  4856  2941  1482  2061
   3042  2690  1159  3702  5786  6349   891   259     1     0     0]
 [ 9482  6923   372  1197  4783  2381  1256  3963   734  9432  5904 16553
   6770  7105   757  8483  5471  2139  1143    59    74  1994     5   276
    178   187  7427     6  1359  2162  1492  2818   422   570   213  2811
   1564  1405   575  1237    36     8     2    15     3     2     3     1
      8     4     4     1     2     3    14     4     4     2     3  1193
    549  3416   412   136  1646    80  1082   102   259  1890  2959  1665
    325   341   198  1741   715  1687  1039  2523  4312  5626  4213   324
    421   122  6214  1858  1643   401  1824  3487   222  1218  3437  1721
   2684  1860  4114   454  1064  1752  3603   327  3343   710  2908  1368
   1595  1664  1358   558   217  3800 10021  8159  1540  4916     1]]
Amount of work: 1628991 3d elements.
NEW WORKER: shape (117, 117, 119) start i =     0. To do:    1628991
2758514936282235 cubes are lit.
```
