#' RHtests process
#' 
#' @param df A data.table with the columns of `site`, `lon`, `lat`, `alt`, `date`
#' and `varname`.
#' @param varname String, the variable that you wish to harmonized.
#' @param overwrite Boolean
#' 
#' @export
process <- function(df = NULL, varname = "Tavg", overwrite = FALSE) {

    file_met = glue("OUTPUT/INPUTS_mete2481_{varname}_daily (195101-201912).rda")

    if (!file.exists(file_met) || overwrite) {
        st_full = df[, .(site, lat, lon, alt, date = make_date(year, month, day))]
        # st_1961_2018 = st_full[date >= "1960-01-01" & date <= "2018-12-31"]
        # 1961_2018, 132 unchanged: lon^2 + lat^2 + alt
        st_moveInfo_raw = get_moveInfo(st_full)
        st_moveInfo = revise_locaiton_multi(st_moveInfo_raw, dist_max = 50)

        st = ddply(st_moveInfo, .(site), . %>% .[which.max(n_period), ])
        coord = st[, .(lon = deg2dec(lon), lat = deg2dec(lat))] %>% as.matrix()
        dist = rdist.earth(coord)

        df2 = df[, .(site, date, Tavg)] %>% fix_uncontinue(complete_year = TRUE)
        save(df2, st_moveInfo, st, dist, file = file_met)
    }
 
    ## 修复了26/51个站点
    sites = st$site %>% as.character()
    nsite = length(sites)
    file_RHtests_daily   = glue("OUTPUT/RHtests_mete{nsite}_{varname}_QMadjusted.RDS")
    file_RHtests_daily_withRef = glue("OUTPUT/RHtests_mete{nsite}_{varname}_QMadjusted withRef.RDS")

    ## 1. PMF test -----------------------------------------------------------------
    lst = df2[, .SD, .SDcols = c("site", "date", varname)] %>% {split(.[, -1], .$site)}
    
    if (!file.exists(file_RHtests_daily) || overwrite) {
        system.time(res <- homo_ref.list(lst, st_moveInfo))
        saveRDS(res, file_RHtests_daily)
    }
    res <- readRDS(file_RHtests_daily)

    TP_info <- TP_merge(res)
    sites_worst = names(TP_info)
    find_refer(df2, st, varname, sites_worst)
    sites_miss = setdiff(sites, d_refs$target) %>% as.character()
    # monthly和yearly一致的，884 TPs
    # temp <- RHtests_main(df2, st_moveInfo = info2_1961_2018, sitename, varname)

    # 剩余的153个站点中，64个存在显著的突变点，21个不存在突变点，其余monthly和yearly数据的突变点不一致
    #  81 not ref-sites
    # 156 not ref-sites, sites_worst
    # 308 not ref-sites, sites_worse
    if (!file.exists(file_RHtests_daily_withRef)) {
        inds = d_refs$target %>% set_names(seq_along(.), .)
        m = nrow(d_refs)
        res_ref = foreach(i = inds[1:m]) %dopar% {
            runningId(i)
            # if (i == 2) break()
            site_target = d_refs$target[i]
            site_refer  = d_refs$site[i]
            i_t = match(site_target, sites)
            i_r = match(site_refer, sites)

            d_target = lst[[i_t]]
            d_refer  = lst[[i_r]]
            d = merge(d_target, d_refer %>% set_names(c("date", "ref")), all.x = TRUE)
            metadata = get_metadata(d, site_target, st_moveInfo)

            tryCatch({
                r = homo_ref(d, metadata)
            }, error = function(e) {
                message(sprintf('[%d] %s', i, e$message))
            })
        }
        saveRDS(res_ref, file = file_RHtests_daily_withRef)
    }
    res_ref = readRDS(file_RHtests_daily_withRef)    
    ## TPs 不为空的站点，采用homo_ref修正；其余的采用no-ref进行修正

    ## 1. 在含有reference sites的站点中---------------------------------------------
    # 2325站点中，1404站点存在突变点，其余921站点保持不变
    TPs = map(res_ref, ~.$day$TP)
    ind_fix = which.notnull(TPs)

    d_ref = map(res_ref[ind_fix], ~.$day$data[, .(date, QM_adjusted)]) %>%
        melt_list("site")

    ## 2. 其余没有reference sites的站点中-------------------------------------------
    # 156站点中，63站点存在突变点
    d_noref = res[sites_miss] %>% map(~.$day$data[, .(date, QM_adjusted)]) %>% rm_empty() %>%
        melt_list("site")
    df_fixed = rbind(d_ref, d_noref) %>% set_colnames(c("site", "date", varname))

    ## merge the unfixed and fixed
    sites_fixed = df_fixed$site %>% unique()

    df_final = rbind(df_fixed, df2[!(site %in% sites_fixed), ])
    date = Sys.Date() %>% format() %>% gsub("-", "", .)
    fwrite(df_final, glue("OUTPUT/OUTPUT_mete2481_{varname}_RHtests_fixed ({date}).csv"))
    df_final
}
