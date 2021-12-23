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
[[1414 2285 3535 ... 4158    1    1]
 [ 856   19  921 ...    1    1    0]
 [ 531  616 1867 ...    0    0    0]]
Amount of work: 1898928560 3d elements.
Sharing the job: spawning 4 workers
NEW WORKER: shape (311, 1240, 1234) start i =     0. To do:  475879760
Done 0.21%
NEW WORKER: shape (310, 1240, 1234) start i =   311. To do:  474349600
Done 0.42%
NEW WORKER: shape (310, 1240, 1234) start i =   621. To do:  474349600
Done 0.63%
NEW WORKER: shape (310, 1240, 1234) start i =   931. To do:  474349600
Done 0.84%
Done 1.1%
Done 1.3%

...

1387966280636636 cubes are lit.
```

Or, on the example input:

```
./advent_22.py 4 < example.txt
[[ 8934   280  9800  2589   341  2334  2289  3720  5472  1326  5876  4457
   2313  5217  3457   910  2968    22  1966  2174   185   718  5264  3217
   3274     1  3186  3146     1   244  1449     1   791  2196  2131     1
    485  6035  2699   445     1  1583    54     1    69  1307     1   556
      1  2266     1  1199     1  1685     1    21     1  3280     1   344
      1   854   682     1  3013     1   201   334   677     5     1     2
      8     1    12     6     9     4     1     5     1     9     1    10
      8     1     1     1     2     1     7     1  1984  6444  2030     1
   1086     1   497     1  1436   459     1   793  1602  1552     1    84
    228     1  3042     1   981     1  2700     1   724  5300  4916     1
    815   783    19     1  2949     1   591  2595  2725     1  3061     1
    627     1  5608   225     1   371     1  1793     1  4664     1  7019
      1   347     1  9169     1  1468     1  3224     1 13968     1   740
      1   683     1  2177     1  3797     1  1783     1  1549     1  2539
      1  2790     1  6857     1     1     0     0     0     0]
 [ 4332 11759  9071   710  8699  8656  5818  3360   446   701  5712  1732
    940  6494  2671  2660     1   851   502   519  2339   730  1461     1
   1033  1299  8592     1  2807  2297     1   993     1  6236   256     1
    456   440   361   829     1   852  1047   727  6654   858     1   310
      1   452     1   805  5747  1295     1     4     1     6     1     1
      1     3     1    16     5    12     1    12     1     1     1     1
     17     2     1     1     1     5     1  2378   259     1   310     1
    412     1  1202   209     1  4271     1  2505     1   779     1   841
      1  2288     1   555  1578   512  4330   159  1690  1405     1  1729
      1  1058     1  1031  1445  1597     1  3401  4414   556    13     1
   2179     1  3786     1  1981     1   380   657     1   118  4068     1
   5752     1   840     1  1973     1  4809     1  1227     1  2055     1
   1202     1  6665     1  4938     1  4855     1  2940     1  1481     1
   2060     1  3041     1  2689     1  1158     1  3701     1  5785     1
   6348     1   890     1   258     1     1     0     0     0]
 [ 9482  6923   372  1197  4783  2381  1256  3963   734  9432  5904 16553
   6770  7105   757  8483  5470     1  2138     1  1142     1    58     1
     74  1994     4     1   275     1   178   187  7427     5     1  1359
   2161     1  1492  2817     1   421     1   570   213  2810     1  1564
   1405   574     1  1237    36     8     2    15     3     2     3     1
      8     3     1     3     1     1     1     1     2     1    13     1
      3     1     3     1     1     1     2     1  1193   548     1  3416
    412   136  1646    80  1082   102   259  1890  2958     1  1665   325
    341   198  1741   714     1  1687  1039  2522     1  4311     1  5626
   4212     1   323     1   420     1   122  6213     1  1857     1  1642
      1   401  1823     1  3487   221     1  1217     1  3436     1  1720
      1  2683     1  1859     1  4113     1   453     1  1063     1  1751
      1  3602     1   326     1  3342     1   709     1  2907     1  1367
      1  1594     1  1663     1  1357     1   557     1   216     1  3799
      1 10020     1  8158     1  1539     1  4915     1     1]]
Amount of work: 5420100 3d elements.
Sharing the job: spawning 4 workers
NEW WORKER: shape (44, 175, 178) start i =     0. To do:    1370600
NEW WORKER: shape (44, 175, 178) start i =    44. To do:    1370600
NEW WORKER: shape (43, 175, 178) start i =    88. To do:    1339450
NEW WORKER: shape (43, 175, 178) start i =   131. To do:    1339450
Done 72.96%
2758514936282235 cubes are lit.
```
