(ns advent-of-code.core)

(def input
  [1587
   1407
   1717
   1596
   1566
   1752
   1925
   1847
   1716
   1726
   1611
   1628
   1853
   1864
   1831
   1942
   1634
   1964
   1603
   1676
   1256
   1906
   1655
   1790
   1666
   1470
   1540
   1544
   1100
   1447
   1384
   1464
   1651
   1572
   907
   1653
   1265
   1510
   1639
   1818
   376
   1378
   1132
   1750
   1491
   1788
   1882
   1779
   1640
   1586
   1525
   1458
   1994
   1782
   1412
   1033
   1416
   1813
   1520
   1968
   715
   1396
   1745
   1506
   1024
   1798
   1870
   1615
   1957
   1718
   1349
   1983
   1387
   1738
   1588
   1321
   1160
   1907
   1861
   1940
   1475
   2004
   1852
   1760
   1608
   1028
   1820
   1495
   1811
   1737
   1417
   1316
   1087
   1803
   1595
   1346
   1971
   1692
   1678
   1330
   1480
   1097
   1898
   1973
   1567
   1733
   1336
   1381
   1327
   1670
   1436
   1989
   1334
   89
   1862
   1715
   1743
   1967
   1765
   1402
   1729
   1749
   1671
   1196
   1650
   1089
   1814
   1783
   1225
   1823
   1746
   2009
   1886
   1748
   1481
   1739
   1912
   1663
   1668
   1314
   1594
   705
   1449
   1731
   1487
   1648
   1466
   1317
   1979
   1799
   1926
   1703
   1656
   1978
   2005
   1865
   1982
   1951
   1892
   1713
   1744
   1598
   1606
   1583
   1895
   1804
   1430
   1816
   1364
   1575
   1918
   1431
   1812
   1471
   1797
   928
   1934
   1156
   94
   1563
   1909
   1453
   1392
   1427
   1819
   1524
   1695
   1866
   2008
   1413
   1698
   1051
   1707
   1904
   1681
   1541
   1621
   1421
   1809
   1576
   ])

; DAY 1
; Find the two entries that sum to 2020; what do you get if you multiply them together?
(defn product-sums [candidate list cond]
  (if (empty? list)
    nil
    (if (= cond (+ candidate (first list)))
      (* candidate (first list))
      (recur candidate (drop 1 list) cond))))

(defn find-pair-product [entries cond]
  (if (empty? entries)
    nil
    (let [head (first entries)
          tail (drop 1 entries)
          result (product-sums head tail cond)]
      (if (some? result)
        result
        (recur tail cond)))))

(find-pair-product input 2020)

; --- Part Two ---
; what is the product of the three entries that sum to 2020?
(defn add-new-sum [sum->entries new-entry checked-entries]
  ; for every entry in checked entries, add entry + new-entry to sum->entries
  (let [new-entry-sums (map #(hash-map (+ new-entry %) [% new-entry]) checked-entries)
        new-entry-sums-map (into {} new-entry-sums)]
    (conj sum->entries new-entry-sums-map)))

(defn find-trio [entries sum-cond]
  (loop [[head & tail] entries
         checked-entries #{}
         sums->entries {}]

    (cond
      ; no more entries to check, return nil
      (nil? head)
      nil

      ; is the complement of head already on the sums?, return entries and head
      (contains? sums->entries (- sum-cond head))
      (conj (sums->entries (- sum-cond head)) head)

    ; update sums->entries with new checked entry and its sums
      :else
      (let [new-sums->entries (add-new-sum sums->entries head checked-entries)
          new-checked-entries (conj checked-entries head)]
        (recur tail new-checked-entries new-sums->entries)))))

(reduce * (find-trio input 2020))