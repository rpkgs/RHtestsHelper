# 1. 数据均一化

- PMF (noRef)  : 不需要参考站
- PMT (withRef): 需要参考站


## 1.1. 参考站挑选规则

### 1.1.1. 确定潜在参考站

  Q1: PMF检测无突变点；<u>（在参考站情况下，大概率也是无突变点）</u>

  Q2: PMF检测有突变点，但站点位置未发生变迁，采用QM-adjusted结果作为reference

### 1.1.2. 挑选每个站点的参考站

  1. 350km范围内, corr(diff(y_target), diff(y_refer)) > 0.7，时间序列最长的站点

    同时需要满足：|HR -HT| ≤200m if HT<2500 m; |HR -HT| ≤500m if HT≥2500 m

  <!-- 2. 如果没有满足的potential reference sites，则采用Q2的站点 -->
  2. 如果经过步骤1，仍均无法找到参考站点，则采用无参考站点的矫正方法(PMF test)

### 1.1.3. PMT实战

测试reference site出现空值对QM_adj的影响


#### 1.1.3.1. 城市化站点挑选

- 突变点显著，RHtests则矫正到Rural (or Urban)，则最终为Rural (or Urban)站点

- 突变点不显著，则采用动态Land Cover划分Urban or Rural


# 2. 数据栅格化


- Tps: 气候态(1981-2010)

- adw: anomaly

脚本放在`spInterp/scripts`
<https://github.com/rpkgs/spInterp>

## 原始数据


说明文档：/mnt/z/GitHub/rpkgs/RHtestsHelper/doc_RHtests_mete2481.md

```r
library(tidymet)
# st_met2481
f_org = "/mnt/z/GitHub/rpkgs/RHtestsHelper/data-raw/INPUT/INPUT_met2474_Tmax&RHmax_for_HImax_1951-2022.fst"
```

## 均一化数据
```r
# RH_avg, Tair_avg, Tair_max
f_homo = "/mnt/z/GitHub/rpkgs/RHtestsHelper/OUTPUT/ChinaHI/OUTPUT_mete2481_1961-2022_RHtests_v20230228_RH_avg.csv"
```
