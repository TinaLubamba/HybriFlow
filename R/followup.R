# ===========================================
# FOLLOW-UP (LONGITUDINAL) ANALYSIS
# ===========================================
# Builds a SEPARATE person-level database (one row per woman) and the follow-up
# figures from final_database. The main database is only READ, never changed.
#
# Everyone is assumed HPV-positive at baseline (enrolment). Visits are ordered by date:
# baseline, follow-up 1, follow-up 2, ...
#
# Settings (see followup_settings()):
#   visits_required : visits in the study protocol (baseline + follow-ups). Default 3.
#   clear_last_k    : used only for the "ended_HPV_negative" column (1 = negative at her
#                     last visit; 2 = negative at her last two visits).
#
# Definitions for a woman with a typed baseline (each baseline genotype is judged separately):
#   persisted : the genotype is detected at baseline and at every follow-up visit
#   cleared   : the genotype is detected at baseline but is not detected at one or more
#               follow-up visits (even if it is detected again later)
#   acquired  : a genotype that was not present at baseline is detected at any follow-up visit
# Three yes/no flags (P = at least one persisted, C = at least one cleared, A = at least one
# acquired) give six mutually exclusive categories: Persist only, Clear only, Persist + clear,
# Persist + acquire, Clear + acquire, Persist + clear + acquire.
# "Persist any", "Clear any" and "Acquire any" are overlapping summaries of the same flags.
# Women whose baseline was universal-probe positive only (untyped) are kept in their own group,
# and women with a universal-probe-only follow-up sample are "Genotype not determined".
# A genotype that disappears and is detected again is classified as cleared; it is also listed
# in "intermittent_types" (and the sample-level "relapse" flag) so that these cases can be found.

#' Follow-up (longitudinal) analysis
#'
#' Builds a separate person-level database (one row per participant) from the
#' sample database and classifies each participant by what happened to her
#' baseline genotypes. A baseline genotype *persisted* if it is detected at
#' baseline and at every follow-up visit, and *cleared* if it is not detected at
#' one or more follow-up visits (even if it is detected again later); a genotype that was
#' not present at baseline and is detected at any follow-up visit was *acquired*.
#' Whether at least one genotype persisted (P), cleared (C) and was acquired (A)
#' gives six mutually exclusive categories: persist only, clear only,
#' persist + clear, persist + acquire, clear + acquire, and
#' persist + clear + acquire. "Persist any", "clear any" and "acquire any" are
#' reported as overlapping summaries. The function writes nine figures, an Excel
#' workbook (`followup_database.xlsx`) and an HTML report
#' (`followup_analysis.html`). The main database is only read, never changed.
#'
#' @param final_database The sample database returned by [run_hybriflow()].
#' @param output_folder Folder to write the outputs to.
#' @param hr_cols,lr_cols Names of the high-risk and low-risk genotype columns
#'   (for example `paste0("HPV_", hpv_genotypes()$high_risk)`).
#' @param settings A list from [followup_settings()].
#' @return Invisibly, a list of the result tables.
#' @export
run_followup_analysis <- function(final_database, output_folder, hr_cols, lr_cols, settings = list()) {
  S <- utils::modifyList(list(visits_required = 3L, clear_last_k = 1L, qc_only = TRUE), settings)
  n_full  <- as.integer(S$visits_required)
  qc_only <- S$qc_only
  stopifnot(n_full >= 2)
  
  geno_cols <- intersect(c(hr_cols, lr_cols), colnames(final_database))
  hr_cols   <- intersect(hr_cols, geno_cols)
  lr_cols   <- intersect(lr_cols, geno_cols)
  as_set <- function(x) if (is.na(x) || x == "") character(0) else strsplit(x, ";", fixed = TRUE)[[1]]
  
  G_PERSIST       <- "Persist only"
  G_CLEAR         <- "Clear only"
  G_PERSIST_CLEAR <- "Persist + clear"
  G_PERSIST_ACQ   <- "Persist + acquire"
  G_CLEAR_ACQ     <- "Clear + acquire"
  G_ALL3          <- "Persist + clear + acquire"
  G_UNTYPED       <- "Untyped baseline (universal probe only)"
  G_NOTDET        <- "Genotype not determined"
  typed6     <- c(G_PERSIST, G_CLEAR, G_PERSIST_CLEAR, G_PERSIST_ACQ, G_CLEAR_ACQ, G_ALL3)
  fig_groups <- c(typed6, G_UNTYPED, G_NOTDET)
  
  # ---- 1. one row per sample with its HPV state ------------------------------
  db <- as.data.frame(final_database, stringsAsFactors = FALSE)
  db$Visit_Date <- as.Date(db$Visit_Date)
  n_all <- nrow(db)
  if (qc_only) db <- db[!is.na(db$Overall_QC_Status) & db$Overall_QC_Status == "PASS", ]
  db <- db[grepl("_", db$Sample_ID, fixed = TRUE), ]          # drops control samples
  if (nrow(db) == 0) stop("No samples available for the follow-up analysis.")
  n_used <- nrow(db)
  db$participant <- sub("_.*$", "", db$Sample_ID)             # 13-digit sample number = the woman
  db$repeat_no   <- repeat_number_from_id(db$Sample_ID)         # 0 = original; ..._7172020-1, ..._070422b = repeats
  
  gm <- as.matrix(db[, geno_cols, drop = FALSE]) == "POSITIVE"
  gm[is.na(gm)] <- FALSE
  db$state <- ifelse(rowSums(gm) > 0, "TYPED",
                     ifelse(!is.na(db$HPV_Universal_Probe) & db$HPV_Universal_Probe == "POSITIVE",
                            "UNIV", "NEG"))
  db$types <- apply(gm, 1, function(r) paste(geno_cols[r], collapse = ";"))
  
  # ---- 2. order the visits by date --------------------------------------------
  participants_all <- unique(db$participant)
  undated <- unique(db$participant[is.na(db$Visit_Date)])
  db <- db[!db$participant %in% undated, ]
  if (nrow(db) == 0) stop("No samples with a readable visit date.")
  # One result per visit date. A repeat is run because the first result was not trusted, so when a
  # visit has an original and repeats, the LATEST repeat is used.
  db <- db[order(db$participant, db$Visit_Date, -db$repeat_no, db$Sample_ID), ]
  db <- db[!duplicated(db[, c("participant", "Visit_Date")]), ]
  db$visit_no <- ave(seq_len(nrow(db)), db$participant, FUN = seq_along)
  db$n_visits <- ave(seq_len(nrow(db)), db$participant, FUN = length)
  
  # visits attended (QC-passed samples): women present at each visit
  max_v <- max(db$visit_no)
  visits_tab <- data.frame(
    visit = c("Baseline", paste0("Follow-up ", seq_len(max_v - 1))[seq_len(max_v - 1)]),
    women = vapply(seq_len(max_v), function(v) length(unique(db$participant[db$visit_no == v])), numeric(1)),
    stringsAsFactors = FALSE)
  visits_tab$pct_of_baseline <- round(100 * visits_tab$women / visits_tab$women[1], 1)
  exact_tab <- as.data.frame(table(visits = db$n_visits[!duplicated(db$participant)]), stringsAsFactors = FALSE)
  names(exact_tab)[2] <- "women"
  exact_text <- paste0("exactly ", exact_tab$visits, " visit(s): ", exact_tab$women, collapse = "; ")
  visits_per_woman <- db$n_visits[!duplicated(db$participant)]   # one entry per woman with a visit order
  
  # ---- 3. classify every woman -------------------------------------------------
  cls_fields <- c("group", "detail", "persist_any", "clear_any", "acquire_any",
                  "persisted_types", "cleared_types", "acquired_types", "intermittent_types",
                  "relapse", "ended_HPV_negative")
  mk <- function(group, detail = NA_character_, ...) {
    out <- stats::setNames(rep(NA_character_, length(cls_fields)), cls_fields)
    out[["group"]]  <- group
    out[["detail"]] <- detail
    extra <- list(...)
    for (nm in names(extra)) out[[nm]] <- extra[[nm]]
    out
  }
  yes_no <- function(x) if (isTRUE(x)) "YES" else "NO"
  
  classify <- function(d) {
    n <- nrow(d); s <- d$state; ty <- lapply(d$types, as_set); B <- ty[[1]]
    if (n == 1)         return(mk("1 visit only"))
    if (s[1] == "NEG")  return(mk("Baseline not positive (check data)"))
    if (n > n_full)     return(mk(paste0("More than ", n_full, " visits (check)")))
    fu <- 2:n
    neg_visits <- which(s == "NEG")
    relapse   <- length(neg_visits) > 0 && any(s != "NEG" & seq_len(n) > min(neg_visits))
    k         <- min(S$clear_last_k, n - 1)
    ended_neg <- all(s[(n - k + 1):n] == "NEG")
    flags  <- list(relapse = yes_no(relapse), ended_HPV_negative = yes_no(ended_neg))
    detail <- NA_character_
    
    if (s[1] == "UNIV") {
      # untyped baseline: genotypes cannot be followed
      grp    <- G_UNTYPED
      detail <- if (s[n] == "NEG") "HPV-negative at last visit" else "HPV-positive at last visit"
      extra  <- flags
    } else if (any(s[fu] == "UNIV")) {
      # a follow-up sample is HPV-positive with no typed genotype: genotypes at that visit unknown
      grp   <- G_NOTDET
      extra <- flags
    } else {
      pers <- B[vapply(B, function(g)                   # detected at EVERY follow-up visit
        all(vapply(ty[fu], function(x) g %in% x, logical(1))), logical(1))]
      clr  <- B[!B %in% pers]                           # missed at one or more follow-ups
      acq  <- setdiff(unique(unlist(ty[fu])), B)        # new genotypes at any follow-up
      inter <- B[vapply(B, function(g) {                # cleared, then detected again later
        p   <- vapply(ty[fu], function(x) g %in% x, logical(1))
        gap <- match(FALSE, p)
        !is.na(gap) && any(p[seq_along(p) > gap])
      }, logical(1))]
      P <- length(pers) > 0; C <- length(clr) > 0; A <- length(acq) > 0
      grp <- if (P && !C && !A) G_PERSIST
      else if (!P && C && !A) G_CLEAR
      else if (P && C && !A)  G_PERSIST_CLEAR
      else if (P && !C && A)  G_PERSIST_ACQ
      else if (!P && C && A)  G_CLEAR_ACQ
      else                    G_ALL3
      coinf  <- if (length(B) == 1) "1 baseline genotype" else "2 or more baseline genotypes"
      detail <- coinf
      if (grp == G_CLEAR && n == n_full) {
        if (s[n] == "NEG") {
          v <- n
          while (v > 2 && s[v - 1] == "NEG") v <- v - 1
          f <- v - 1
          when <- if (f == 1) "early (negative from first follow-up)" else paste0("late (negative from follow-up ", f, ")")
        } else {
          when <- "detected again after a negative follow-up"
        }
        detail <- paste0(coinf, "; ", when)
      }
      extra <- c(flags, list(persist_any = yes_no(P), clear_any = yes_no(C), acquire_any = yes_no(A),
                             persisted_types = paste(pers, collapse = ";"),
                             cleared_types = paste(clr, collapse = ";"),
                             acquired_types = paste(acq, collapse = ";"),
                             intermittent_types = paste(inter, collapse = ";")))
    }
    if (n < n_full) grp <- paste0(n, " visits: ", grp, " (so far)")
    do.call(mk, c(list(grp, detail), extra))
  }
  
  res <- lapply(split(db, db$participant), classify)
  cls <- data.frame(participant = names(res), do.call(rbind, res),
                    stringsAsFactors = FALSE, row.names = NULL)
  if (length(undated) > 0) {
    add <- data.frame(participant = undated, group = "Visit date unreadable (cannot order visits)",
                      detail = NA_character_, stringsAsFactors = FALSE)
    for (cn in setdiff(names(cls), names(add))) add[[cn]] <- NA_character_
    cls <- rbind(cls, add[, names(cls)])
  }
  
  abbr <- c(TYPED = "P", UNIV = "U", NEG = "N")
  feat <- do.call(rbind, lapply(split(db, db$participant), function(d) {
    B   <- as_set(d$types[1])
    new <- setdiff(unique(unlist(lapply(d$types[-1], as_set))), B)
    data.frame(participant = d$participant[1], n_visits = nrow(d),
               pattern = paste(abbr[d$state], collapse = " > "),
               baseline_year = format(d$Visit_Date[1], "%Y"),
               baseline_types = d$types[1],
               n_baseline_types = length(B),
               baseline_HR = any(B %in% hr_cols), baseline_LR = any(B %in% lr_cols),
               new_HR = any(new %in% hr_cols), new_LR = any(new %in% lr_cols),
               followup_days = as.numeric(max(d$Visit_Date) - min(d$Visit_Date)),
               stringsAsFactors = FALSE)
  }))
  feat$baseline_risk <- ifelse(feat$baseline_HR & feat$baseline_LR, "Both HR and LR",
                               ifelse(feat$baseline_HR, "HR only",
                                      ifelse(feat$baseline_LR, "LR only", "Untyped")))
  followup_db <- merge(cls, feat, by = "participant", all.x = TRUE)
  
  # ---- 4. summary tables -------------------------------------------------------
  n_women <- nrow(followup_db)
  grp_tab <- as.data.frame(table(group = followup_db$group), stringsAsFactors = FALSE)
  names(grp_tab)[2] <- "n"
  grp_tab$pct <- round(100 * grp_tab$n / sum(grp_tab$n), 1)
  grp_tab <- grp_tab[order(-grp_tab$n), ]
  
  sub_in <- followup_db[!is.na(followup_db$detail), c("group", "detail")]
  subtype_tab <- if (nrow(sub_in) > 0) {
    st <- as.data.frame(table(group = sub_in$group, detail = sub_in$detail), stringsAsFactors = FALSE)
    st <- st[st$Freq > 0, ]
    names(st)[names(st) == "Freq"] <- "n"
    st
  } else data.frame(group = character(0), detail = character(0), n = integer(0))
  
  pattern_tab <- as.data.frame(sort(table(followup_db$pattern), decreasing = TRUE), stringsAsFactors = FALSE)
  names(pattern_tab) <- c("visit_pattern", "n")
  
  # women with complete follow-up, a typed baseline and a classified outcome
  keep_rows <- which(followup_db$n_visits %in% n_full & followup_db$group %in% c(typed6, G_NOTDET) &
                       followup_db$baseline_risk %in% c("HR only", "LR only", "Both HR and LR"))
  three <- followup_db[keep_rows, ]
  
  n_five <- unname(vapply(fig_groups, function(g) sum(followup_db$group == g), numeric(1)))
  fig1_tab <- data.frame(
    bar = c("Women (participants)", fig_groups, "Other"),
    n   = c(n_women, n_five, n_women - sum(n_five)),
    stringsAsFactors = FALSE)
  fig1_tab$pct <- round(100 * fig1_tab$n / n_women, 1)
  other_tab <- grp_tab[!grp_tab$group %in% fig_groups, ]
  other_text <- if (nrow(other_tab) > 0) paste0(other_tab$group, " (", other_tab$n, ")", collapse = "; ") else "none"
  
  flow_tab <- data.frame(
    step = c("Samples in the main database", "Samples used (QC-passed, not controls)",
             "Women (participants)", "Women whose visit dates can all be read",
             paste0("  with ", seq_len(n_full), " visit(s)"),
             paste0("  with more than ", n_full, " visits"),
             paste0("Women with ", n_full, " visits and a typed baseline")),
    n = c(n_all, n_used, length(participants_all), length(unique(db$participant)),
          vapply(seq_len(n_full), function(k) sum(followup_db$n_visits %in% k), numeric(1)),
          sum(followup_db$n_visits > n_full, na.rm = TRUE), nrow(three)),
    stringsAsFactors = FALSE)
  
  # Figure 4: the six categories by baseline infection
  t4 <- three[three$group %in% typed6, ]
  n_notdet_excluded <- sum(three$group == G_NOTDET)
  f3 <- data.frame()
  if (nrow(t4) > 0) {
    f3 <- as.data.frame(table(baseline_risk = t4$baseline_risk, group = t4$group), stringsAsFactors = FALSE)
    names(f3)[3] <- "n"
    f3$total <- ave(f3$n, f3$baseline_risk, FUN = sum)
    f3$pct <- round(100 * f3$n / f3$total, 1)
    f3 <- f3[f3$n > 0, ]
    f3$risk_lab <- paste0(f3$baseline_risk, "\n(n=", f3$total, ")")
    f3$group <- factor(f3$group, levels = typed6)
  }
  
  # Figure 5: persistence vs clearance (women with complete follow-up who ended in one of the two)
  pc <- followup_db[followup_db$n_visits %in% n_full & followup_db$group %in% c(G_PERSIST, G_CLEAR), ]
  compare_tab <- data.frame()
  if (nrow(pc) > 0) {
    pc$infection <- ifelse(pc$baseline_risk == "Untyped", "Untyped (universal probe only)", pc$baseline_risk)
    pc$coinf <- ifelse(pc$n_baseline_types == 0, "None typed",
                       ifelse(pc$n_baseline_types == 1, "1 genotype", "2 or more genotypes"))
    pc$grp <- factor(pc$group, levels = c(G_PERSIST, G_CLEAR))
    
    make_block <- function(var, panel_name, level_order = NULL) {
      lv <- if (is.null(level_order)) sort(unique(pc[[var]])) else level_order[level_order %in% pc[[var]]]
      tb <- table(factor(pc[[var]], levels = lv), pc$grp)
      p <- tryCatch(fisher.test(tb)$p.value,
                    error = function(e) tryCatch(fisher.test(tb, simulate.p.value = TRUE, B = 10000)$p.value,
                                                 error = function(e2) NA_real_))
      data.frame(panel = panel_name, level = lv,
                 n_persist = as.integer(tb[, 1]), n_clear = as.integer(tb[, 2]),
                 pct_persist = round(100 * tb[, 1] / sum(tb[, 1]), 1),
                 pct_clear   = round(100 * tb[, 2] / sum(tb[, 2]), 1),
                 p_value = c(p, rep(NA_real_, length(lv) - 1)), stringsAsFactors = FALSE)
    }
    compare_tab <- rbind(
      make_block("infection", "Baseline infection",
                 c("HR only", "LR only", "Both HR and LR", "Untyped (universal probe only)")),
      make_block("coinf", "Co-infection at baseline", c("1 genotype", "2 or more genotypes", "None typed")))
  }
  
  # ---- woman-level categories by number of baseline genotypes, and the overlapping 'any' summaries
  typed_full <- followup_db[followup_db$n_visits %in% n_full & followup_db$group %in% typed6, ]
  cat_tab <- data.frame(); any_tab <- data.frame()
  if (nrow(typed_full) > 0) {
    one_g <- typed_full$n_baseline_types == 1
    cat_tab <- data.frame(
      category = typed6,
      n_1_baseline_genotype      = unname(vapply(typed6, function(g) sum(typed_full$group == g & one_g), numeric(1))),
      n_2plus_baseline_genotypes = unname(vapply(typed6, function(g) sum(typed_full$group == g & !one_g), numeric(1))),
      stringsAsFactors = FALSE)
    cat_tab$total <- cat_tab$n_1_baseline_genotype + cat_tab$n_2plus_baseline_genotypes
    cat_tab$pct_of_women <- round(100 * cat_tab$total / sum(cat_tab$total), 1)
    any_n <- c(sum(typed_full$persist_any == "YES"), sum(typed_full$clear_any == "YES"), sum(typed_full$acquire_any == "YES"))
    any_tab <- data.frame(
      summary = c("Persist any (at least one baseline genotype persisted)",
                  "Clear any (at least one baseline genotype cleared)",
                  "Acquire any (at least one new genotype detected)"),
      women = any_n, out_of = nrow(typed_full), pct = round(100 * any_n / nrow(typed_full), 1),
      stringsAsFactors = FALSE)
  }
  
  # ---- genotype-level processes: persistence, clearance, acquisition --------------
  # Women with complete follow-up and a typed baseline. A woman whose follow-up sample was
  # universal-probe positive with no typed genotype is left out of these figures,
  # because her genotypes at that visit are unknown (missing, not cleared).
  db3 <- db[db$participant %in% three$participant, ]
  wl <- lapply(split(db3, db3$participant), function(d) {
    list(id = d$participant[1], S = lapply(d$types, as_set), univ_fu = any(d$state[-1] == "UNIV"))
  })
  wl_ok <- Filter(function(w) !w$univ_fu && length(w$S) == n_full, wl)
  n_geno_women   <- length(wl_ok)
  n_geno_missing <- length(wl) - n_geno_women
  risk_of <- function(g) ifelse(g %in% hr_cols, "High-risk", "Low-risk")
  
  F_PERS_ALL  <- "Persisted (detected at every follow-up)"
  F_CLR_EARLY <- "Cleared early (gone at first follow-up)"
  F_CLR_LATE  <- "Cleared late (gone at a later follow-up)"
  fate_class <- function(p) {          # p = TRUE/FALSE for each follow-up visit
    if (all(p)) return(F_PERS_ALL)                  # persisted = detected at every follow-up
    if (!p[1]) F_CLR_EARLY else F_CLR_LATE          # cleared = missed at one or more follow-ups
  }
  fate <- do.call(rbind, lapply(wl_ok, function(w) {
    B <- w$S[[1]]
    if (!length(B)) return(NULL)
    data.frame(participant = w$id, genotype = B,
               fate = unname(vapply(B, function(g) fate_class(vapply(w$S[-1], function(x) g %in% x, logical(1))), character(1))),
               stringsAsFactors = FALSE)
  }))
  fate_tab <- data.frame()
  if (!is.null(fate) && nrow(fate) > 0) {
    fate$risk <- risk_of(fate$genotype)
    fate_tab <- as.data.frame.matrix(table(fate$genotype, fate$fate))
    fate_tab <- cbind(genotype = rownames(fate_tab), fate_tab)
    rownames(fate_tab) <- NULL
  }
  
  acq_tab <- data.frame()
  if (n_geno_women > 0) {
    all_B   <- unlist(lapply(wl_ok, function(w) w$S[[1]]))
    acq_ev  <- unlist(lapply(wl_ok, function(w) setdiff(unique(unlist(w$S[-1])), w$S[[1]])))
    n_base  <- as.integer(table(factor(all_B, levels = geno_cols)))
    n_acq   <- as.integer(table(factor(acq_ev, levels = geno_cols)))
    acq_tab <- data.frame(genotype = geno_cols, risk = risk_of(geno_cols),
                          at_risk = n_geno_women - n_base, newly_detected = n_acq,
                          stringsAsFactors = FALSE)
    acq_tab$pct <- round(100 * acq_tab$newly_detected / acq_tab$at_risk, 1)
  }
  
  trans_tab <- data.frame()
  if (n_geno_women > 0) {
    trans_tab <- do.call(rbind, lapply(seq_len(n_full - 1), function(t) {
      P <- sum(vapply(wl_ok, function(w) length(intersect(w$S[[t]], w$S[[t + 1]])), numeric(1)))
      C <- sum(vapply(wl_ok, function(w) length(setdiff(w$S[[t]], w$S[[t + 1]])), numeric(1)))
      A <- sum(vapply(wl_ok, function(w) length(setdiff(w$S[[t + 1]], w$S[[t]])), numeric(1)))
      tr <- if (t == 1) "Baseline to follow-up 1" else paste0("Follow-up ", t - 1, " to follow-up ", t)
      data.frame(transition = tr, event = c("Persisted", "Cleared", "Newly detected"),
                 n = c(P, C, A),
                 pct_of_present = c(round(100 * P / (P + C), 1), round(100 * C / (P + C), 1), NA),
                 stringsAsFactors = FALSE)
    }))
  }
  
  ci_pct <- function(k, n) if (n == 0) c(NA_real_, NA_real_) else round(100 * binom.test(k, n)$conf.int, 1)
  risk_rates <- data.frame()
  if (!is.null(fate) && nrow(fate) > 0) {
    rows <- list()
    for (rk in c("High-risk", "Low-risk")) {
      f <- fate[fate$risk == rk, ]
      n <- nrow(f)
      k_p <- sum(f$fate %in% F_PERS_ALL)
      k_c <- sum(f$fate %in% c(F_CLR_EARLY, F_CLR_LATE))
      ids_new <- vapply(wl_ok, function(w) any(setdiff(unique(unlist(w$S[-1])), w$S[[1]]) %in%
                                                 (if (rk == "High-risk") hr_cols else lr_cols)), logical(1))
      k_a <- sum(ids_new); n_a <- length(wl_ok)
      rows[[length(rows) + 1]] <- data.frame(process = "Persistence of baseline genotypes", risk = rk, k = k_p, n = n,
                                             pct = ifelse(n > 0, round(100 * k_p / n, 1), NA), ci_low = ci_pct(k_p, n)[1], ci_high = ci_pct(k_p, n)[2])
      rows[[length(rows) + 1]] <- data.frame(process = "Clearance of baseline genotypes", risk = rk, k = k_c, n = n,
                                             pct = ifelse(n > 0, round(100 * k_c / n, 1), NA), ci_low = ci_pct(k_c, n)[1], ci_high = ci_pct(k_c, n)[2])
      rows[[length(rows) + 1]] <- data.frame(process = "Women with a newly detected genotype", risk = rk, k = k_a, n = n_a,
                                             pct = round(100 * k_a / n_a, 1), ci_low = ci_pct(k_a, n_a)[1], ci_high = ci_pct(k_a, n_a)[2])
    }
    risk_rates <- do.call(rbind, rows)
  }
  
  pool_ids <- followup_db$baseline_types[!is.na(followup_db$baseline_types) & followup_db$baseline_types != ""]
  pool_tab <- data.frame()
  if (length(pool_ids) > 0) {
    pool_counts <- table(unlist(lapply(pool_ids, as_set)))
    pool_tab <- data.frame(genotype = names(pool_counts), women = as.integer(pool_counts), stringsAsFactors = FALSE)
    pool_tab$risk <- risk_of(pool_tab$genotype)
    pool_tab <- pool_tab[order(-pool_tab$women), ]
    pool_tab$pct_of_women <- round(100 * pool_tab$women / length(pool_ids), 1)
  }
  
  prev <- data.frame()
  if (nrow(db3) > 0) {
    db3$any_HPV <- db3$state != "NEG"
    db3$has_HR <- rowSums(db3[, hr_cols, drop = FALSE] == "POSITIVE", na.rm = TRUE) > 0
    db3$has_LR <- rowSums(db3[, lr_cols, drop = FALSE] == "POSITIVE", na.rm = TRUE) > 0
    prev <- data.frame(visit = seq_len(n_full),
                       any_HPV   = sapply(seq_len(n_full), function(v) round(100 * mean(db3$any_HPV[db3$visit_no == v]), 1)),
                       high_risk = sapply(seq_len(n_full), function(v) round(100 * mean(db3$has_HR[db3$visit_no == v]), 1)),
                       low_risk  = sapply(seq_len(n_full), function(v) round(100 * mean(db3$has_LR[db3$visit_no == v]), 1)))
  }
  
  # ---- 5. console output -------------------------------------------------------
  message("\n===== FOLLOW-UP ANALYSIS =====")
  cat("\n--- Visits attended (QC-passed samples) ---\n"); print(visits_tab, row.names = FALSE)
  cat("Women with unreadable dates:", length(undated), "|", exact_text, "\n")
  cat("\n--- Figure 2 counts ---\n");               print(fig1_tab, row.names = FALSE)
  cat("\n'Other' is made up of:", other_text, "\n")
  cat("\n--- All groups ---\n");                    print(grp_tab, row.names = FALSE)
  if (nrow(f3) > 0) { cat("\n--- Figure 4: outcome by baseline infection ---\n"); print(f3[, c("baseline_risk", "group", "n", "total", "pct")], row.names = FALSE) }
  cat("\nWomen with genotype not determined (not shown in Figure 4):", n_notdet_excluded, "\n")
  if (nrow(cat_tab) > 0) { cat("\n--- Categories by number of baseline genotypes ---\n"); print(cat_tab, row.names = FALSE) }
  if (nrow(any_tab) > 0) { cat("\n--- 'Any' summaries (overlapping, do not add to 100%) ---\n"); print(any_tab, row.names = FALSE) }
  if (nrow(compare_tab) > 0) { cat("\n--- Figure 5: persist only vs clear only ---\n"); print(compare_tab, row.names = FALSE) }
  cat("\nGenotype-level figures use", n_geno_women, "women (", n_geno_missing,
      "left out because a follow-up sample had no typed genotype)\n")
  if (nrow(risk_rates) > 0) { cat("\n--- Figure 9: high-risk vs low-risk rates ---\n"); print(risk_rates, row.names = FALSE) }
  if (nrow(trans_tab) > 0)  { cat("\n--- Figure 8: transitions between visits ---\n"); print(trans_tab, row.names = FALSE) }
  
  # ---- 6. figures --------------------------------------------------------------
  fig_dir <- file.path(output_folder, "followup_figures")
  dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
  save_fig <- function(p, name, w = 9, h = 5.5) {
    # Long captions would run off the edges: wrap them to the width of the figure, left-aligned.
    cap <- p$labels$caption
    if (is.character(cap) && length(cap) == 1 && !is.na(cap)) {
      width <- max(40, floor((w - 1) / 0.085))
      wrapped <- paste(vapply(strsplit(cap, "\n", fixed = TRUE)[[1]],
                              function(l) stringr::str_wrap(l, width = width), character(1)), collapse = "\n")
      p <- p + ggplot2::labs(caption = wrapped)
    }
    p <- p + ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0),
                            plot.caption.position = "plot")
    ggplot2::ggsave(file.path(fig_dir, name), p, width = w, height = h, dpi = 200)
    name
  }
  th <- ggplot2::theme_minimal(base_size = 12)
  group_cols <- c("Persist only" = "#D32F2F", "Clear only" = "#388E3C",
                  "Persist + clear" = "#F57C00", "Persist + acquire" = "#7B1FA2",
                  "Clear + acquire" = "#1E88E5", "Persist + clear + acquire" = "#795548",
                  "Untyped baseline (universal probe only)" = "#BDBDBD",
                  "Genotype not determined" = "#9E9E9E")
  figs <- list()
  
  # Figure 1: participant flow (how the visit counts relate to each other)
  full_women  <- followup_db[followup_db$n_visits %in% n_full, ]
  aside_women <- full_women[!full_women$participant %in% t4$participant, ]
  aside_label <- function(g) ifelse(g == G_NOTDET, "genotype not determined",
                   ifelse(g == G_UNTYPED, "baseline typed by universal probe only",
                   ifelse(grepl("^Baseline not positive", g), "baseline not HPV-positive",
                          tolower(g))))
  set_aside <- if (nrow(aside_women)) table(aside_label(aside_women$group)) else integer(0)
  if (length(set_aside)) {
    set_aside <- stats::setNames(as.integer(set_aside), names(set_aside))
    set_aside <- set_aside[order(-set_aside, names(set_aside))]
  } else {
    set_aside <- integer(0)
  }
  figs$flow <- save_fig(
    plot_participant_flow(length(participants_all), length(undated), visits_per_woman,
                          n_full = n_full, n_outcome = nrow(t4), set_aside = set_aside),
    "fig1_participant_flow.png", 10, 6.8)
  
  # Figure 2: follow-up outcome of women with complete follow-up (six categories, % of those women)
  f2 <- data.frame(category = typed6,
                   n = unname(vapply(typed6, function(g) sum(followup_db$group == g), numeric(1))),
                   stringsAsFactors = FALSE)
  f2_total <- sum(f2$n)
  f2$pct <- if (f2_total > 0) round(100 * f2$n / f2_total, 1) else 0
  f2$category <- factor(f2$category, levels = typed6)
  n_f2_notdet   <- sum(followup_db$group == G_NOTDET)
  n_f2_untyped  <- sum(followup_db$group == G_UNTYPED)
  n_f2_other    <- fig1_tab$n[fig1_tab$bar == "Other"]
  f2_note <- paste0("Not shown: ", n_f2_notdet, " with genotype not determined, ", n_f2_untyped,
                    " with an untyped baseline, and ", n_f2_other, " without complete follow-up.")
  figs$groups <- save_fig(
    ggplot2::ggplot(f2, ggplot2::aes(x = category, y = pct, fill = category)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(n, "\n(", pct, "%)")), vjust = -0.3, size = 3.6) +
      ggplot2::scale_fill_manual(values = group_cols) +
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, 12)) +
      ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"),
                                  expand = ggplot2::expansion(mult = c(0, 0.18))) +
      ggplot2::labs(title = paste0("Follow-up outcome of women with all ", n_full, " visits"),
                    x = NULL, y = "% of women",
                    caption = paste0("Percentages are of the ", f2_total, " women with all ", n_full,
                                     " visits and a typed baseline (each woman is in one category).\n", f2_note)) + th,
    "fig2_followup_groups.png", 10, 6)
  
  # Figure 3: genotypes present at enrolment
  if (nrow(pool_tab) > 0) {
    pool_tab$genotype <- factor(pool_tab$genotype, levels = pool_tab$genotype)
    figs$pool <- save_fig(
      ggplot2::ggplot(pool_tab, ggplot2::aes(x = genotype, y = pct_of_women, fill = risk)) +
        ggplot2::geom_col() +
        ggplot2::geom_text(ggplot2::aes(label = paste0(women, "\n(", pct_of_women, "%)")), vjust = -0.3, size = 2.7, lineheight = 0.9) +
        ggplot2::scale_fill_manual(values = c("High-risk" = "#D32F2F", "Low-risk" = "#1E88E5")) +
        ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), expand = ggplot2::expansion(mult = c(0, 0.15))) +
        ggplot2::labs(title = "Genotypes present at enrolment (baseline)", x = NULL,
                      y = "% of women with a typed baseline", fill = NULL,
                      caption = paste0("Women with a typed baseline: ", length(pool_ids),
                                       ". Each bar is labelled with the number of women and, in brackets, the percentage. A woman with several genotypes is counted once for each, so the percentages add to more than 100%.")) +
        th + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)),
      "fig3_genotypes_at_baseline.png", 12, 6)
  }
  base_tab <- as.data.frame(table(baseline_risk = followup_db$baseline_risk[!is.na(followup_db$baseline_risk)]),
                            stringsAsFactors = FALSE)
  base_text <- if (nrow(base_tab) > 0) paste0(base_tab$baseline_risk, " (", base_tab$Freq, ")", collapse = "; ") else "none"
  
  # Figure 4: outcome by baseline infection
  if (nrow(f3) > 0) {
    figs$outcome <- save_fig(
      ggplot2::ggplot(f3, ggplot2::aes(x = risk_lab, y = pct, fill = group)) +
        ggplot2::geom_col() +
        ggplot2::geom_text(ggplot2::aes(label = paste0(pct, "%")),
                           position = ggplot2::position_stack(vjust = 0.5), colour = "white", size = 4) +
        ggplot2::scale_fill_manual(values = group_cols, drop = TRUE) +
        ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%")) +
        ggplot2::labs(title = paste0("Outcome by baseline infection (women with ", n_full, " visits)"),
                      x = "Baseline infection", y = "Share of women", fill = "Outcome") + th,
      "fig4_outcome_by_baseline_risk.png", 9, 6)
  }
  
  # Figure 5: persistence compared with clearance
  if (nrow(compare_tab) > 0) {
    n_p <- sum(pc$group == G_PERSIST); n_c <- sum(pc$group == G_CLEAR)
    lab_p <- paste0("Persist only (n=", n_p, ")"); lab_c <- paste0("Clear only (n=", n_c, ")")
    f4 <- rbind(
      data.frame(panel = compare_tab$panel, level = compare_tab$level, group_lab = lab_p,
                 pct = compare_tab$pct_persist, n = compare_tab$n_persist, stringsAsFactors = FALSE),
      data.frame(panel = compare_tab$panel, level = compare_tab$level, group_lab = lab_c,
                 pct = compare_tab$pct_clear, n = compare_tab$n_clear, stringsAsFactors = FALSE))
    f4$panel     <- factor(f4$panel, levels = unique(compare_tab$panel))
    f4$level     <- factor(f4$level, levels = unique(compare_tab$level))
    f4$group_lab <- factor(f4$group_lab, levels = c(lab_p, lab_c))
    fill_vals <- stats::setNames(c("#D32F2F", "#388E3C"), c(lab_p, lab_c))
    figs$compare <- save_fig(
      ggplot2::ggplot(f4, ggplot2::aes(x = level, y = pct, fill = group_lab)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.9)) +
        ggplot2::geom_text(ggplot2::aes(label = paste0(pct, "%\n(", n, ")")),
                           position = ggplot2::position_dodge(width = 0.9), vjust = -0.2, size = 2.7) +
        ggplot2::facet_wrap(~ panel, scales = "free_x", ncol = 2) +
        ggplot2::scale_fill_manual(values = fill_vals) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.2))) +
        ggplot2::labs(title = "Persist only compared with clear only (woman level)",
                      x = NULL, y = "% of the women in each group", fill = NULL,
                      caption = "Each bar is the share of that group's women (number of women in brackets).") +
        th + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1)),
      "fig5_persistence_vs_clearance.png", 9, 6)
  }
  
  # Figure 6: persistence and clearance of each baseline genotype
  fate_cols <- c("Persisted (detected at every follow-up)" = "#D32F2F",
                 "Cleared early (gone at first follow-up)" = "#388E3C",
                 "Cleared late (gone at a later follow-up)" = "#A5D6A7")
  if (!is.null(fate) && nrow(fate) > 0) {
    n_by_g <- table(fate$genotype)
    ord <- names(sort(n_by_g, decreasing = TRUE))
    fate$genotype <- factor(fate$genotype, levels = ord)
    fate$fate <- factor(fate$fate, levels = names(fate_cols))
    fate_n <- data.frame(genotype = factor(ord, levels = ord), n = as.integer(n_by_g[ord]), stringsAsFactors = FALSE)
    fate_n$risk <- risk_of(as.character(fate_n$genotype))
    figs$fate <- save_fig(
      ggplot2::ggplot(fate, ggplot2::aes(x = genotype, fill = fate)) +
        ggplot2::geom_bar(position = "fill") +
        ggplot2::geom_text(data = fate_n, ggplot2::aes(x = genotype, y = 1.04, label = n),
                           inherit.aes = FALSE, size = 3) +
        ggplot2::facet_grid(. ~ risk, scales = "free_x", space = "free_x") +
        ggplot2::scale_y_continuous(labels = scales::percent, expand = ggplot2::expansion(mult = c(0, 0.08))) +
        ggplot2::scale_fill_manual(values = fate_cols, drop = FALSE) +
        ggplot2::labs(title = "Persistence and clearance of each baseline genotype",
                      x = NULL, y = "Share of women with that genotype at baseline", fill = NULL,
                      caption = paste0("Number above each bar = women with that genotype at baseline (", n_geno_women, " women with ", n_full, " visits).")) +
        th + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5), legend.position = "bottom"),
      "fig6_genotype_persistence_clearance.png", 12, 6.5)
  }
  
  # Figure 7: acquisition
  if (nrow(acq_tab) > 0 && any(acq_tab$newly_detected > 0)) {
    a6 <- acq_tab[acq_tab$newly_detected > 0, ]
    a6 <- a6[order(-a6$newly_detected), ]
    a6$genotype <- factor(a6$genotype, levels = a6$genotype)
    figs$acq <- save_fig(
      ggplot2::ggplot(a6, ggplot2::aes(x = genotype, y = pct, fill = risk)) +
        ggplot2::geom_col() +
        ggplot2::geom_text(ggplot2::aes(label = paste0(newly_detected, "/", at_risk)), vjust = -0.3, size = 3) +
        ggplot2::facet_grid(. ~ risk, scales = "free_x", space = "free_x") +
        ggplot2::scale_fill_manual(values = c("High-risk" = "#D32F2F", "Low-risk" = "#1E88E5"), guide = "none") +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
        ggplot2::labs(title = "Newly detected genotypes at follow-up (acquisition)",
                      x = NULL, y = "% of women without the genotype at baseline",
                      caption = "Label = women with the genotype newly detected / women without it at baseline. A genotype below the detection limit at baseline cannot be told apart from a new infection.") +
        th + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)),
      "fig7_genotype_acquisition.png", 11, 5.5)
  }
  
  # Figure 8: transitions between visits
  if (nrow(trans_tab) > 0) {
    tr <- trans_tab
    tr$event <- factor(tr$event, levels = c("Persisted", "Cleared", "Newly detected"))
    tr$transition <- factor(tr$transition, levels = unique(trans_tab$transition))
    tr$lab <- ifelse(is.na(tr$pct_of_present), as.character(tr$n), paste0(tr$n, "\n(", tr$pct_of_present, "%)"))
    figs$trans <- save_fig(
      ggplot2::ggplot(tr, ggplot2::aes(x = event, y = n, fill = event)) +
        ggplot2::geom_col(show.legend = FALSE) +
        ggplot2::geom_text(ggplot2::aes(label = lab), vjust = -0.3, size = 3.4) +
        ggplot2::facet_wrap(~ transition) +
        ggplot2::scale_fill_manual(values = c("Persisted" = "#D32F2F", "Cleared" = "#388E3C", "Newly detected" = "#1E88E5")) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.2))) +
        ggplot2::labs(title = "Genotype events between consecutive visits", x = NULL,
                      y = "Genotype events (woman x genotype)",
                      caption = "Percentages are of the genotypes present at the earlier visit (persisted + cleared = 100%).") + th,
      "fig8_visit_transitions.png", 10 + 3 * max(0, n_full - 3), 5.5)
  }
  
  # Figure 9: high-risk against low-risk
  if (nrow(risk_rates) > 0) {
    rr <- risk_rates
    rr$process <- factor(rr$process, levels = c("Persistence of baseline genotypes",
                                                "Clearance of baseline genotypes",
                                                "Women with a newly detected genotype"))
    rr$risk <- factor(rr$risk, levels = c("High-risk", "Low-risk"))
    figs$risk <- save_fig(
      ggplot2::ggplot(rr, ggplot2::aes(x = process, y = pct, fill = risk)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.9)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = ci_low, ymax = ci_high),
                               position = ggplot2::position_dodge(width = 0.9), width = 0.2) +
        ggplot2::geom_text(ggplot2::aes(y = ifelse(is.na(ci_high), pct, ci_high), label = paste0(pct, "% (", k, "/", n, ")")),
                           position = ggplot2::position_dodge(width = 0.9), vjust = -0.5, size = 3) +
        ggplot2::scale_fill_manual(values = c("High-risk" = "#D32F2F", "Low-risk" = "#1E88E5")) +
        ggplot2::scale_x_discrete(labels = function(x) gsub(" ", "\n", x)) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.2))) +
        ggplot2::labs(title = "High-risk compared with low-risk genotypes", x = NULL,
                      y = "%  (with 95% CI)", fill = NULL,
                      caption = "Persistence and clearance: of baseline genotypes (a woman with several genotypes counts once for each). Acquisition: % of women with a newly detected genotype of that risk group.") + th,
      "fig9_high_vs_low_risk.png", 11, 6)
  }
  
  # ---- 7. separate follow-up database (Excel) -----------------------------------
  compare_show <- data.frame()
  if (nrow(compare_tab) > 0) {
    compare_show <- data.frame(
      Characteristic = compare_tab$panel,
      Level = compare_tab$level,
      `Persist only` = paste0(compare_tab$n_persist, " (", compare_tab$pct_persist, "%)"),
      `Clear only`   = paste0(compare_tab$n_clear, " (", compare_tab$pct_clear, "%)"),
      p_value = ifelse(is.na(compare_tab$p_value), "", formatC(compare_tab$p_value, format = "g", digits = 2)),
      stringsAsFactors = FALSE)
  }
  
  settings_lines <- c(
    paste0("Visits per woman in the study (baseline + follow-ups): ", n_full),
    "A baseline genotype is persisted if it is detected at baseline and at every follow-up visit, and cleared if it is not detected at one or more follow-up visits (even if it is detected again later).",
    "A genotype is acquired if it was not present at baseline and is detected at any follow-up visit.",
    "P = at least one baseline genotype persisted; C = at least one cleared; A = at least one acquired. The six categories (Persist only, Clear only, Persist + clear, Persist + acquire, Clear + acquire, Persist + clear + acquire) are mutually exclusive: each woman falls into exactly one.",
    "Persist any, Clear any and Acquire any are overlapping summaries: a woman can be in more than one.",
    "Women whose baseline was universal-probe positive only (untyped) are kept as their own group; women with a follow-up sample that was universal-probe positive but untyped are 'Genotype not determined'.",
    paste0("'Ended HPV-negative' column: HPV-negative at her last ", if (S$clear_last_k == 1) "visit" else paste0(S$clear_last_k, " visits")),
    paste0("Samples used: ", if (qc_only) "QC-passed only" else "all"))
  
  xlsx_path <- file.path(output_folder, "followup_database.xlsx")
  openxlsx::write.xlsx(
    list(Settings_used = data.frame(setting = settings_lines),
         Participants = followup_db, Visits_attended = visits_tab, Figure2_counts = fig1_tab,
         Group_summary = grp_tab, Genotype_pool = pool_tab, Outcome_by_baseline = f3,
         PersistOnly_vs_ClearOnly = compare_tab, Category_by_coinfection = cat_tab, Any_summary = any_tab,
         Genotype_fate = fate_tab,
         Genotype_acquisition = acq_tab, Visit_transitions = trans_tab,
         Risk_group_rates = risk_rates, Cohort_flow = flow_tab, Subtypes = subtype_tab,
         Visit_patterns = pattern_tab, Prevalence_by_visit = prev),
    file = xlsx_path, overwrite = TRUE)
  
  # ---- 8. HTML report -------------------------------------------------------------
  tbl <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(htmltools::tags$p("No data."))
    cell <- function(v) htmltools::tags$td(ifelse(is.na(v), "", as.character(v)),
                                           style = "border:1px solid #ccc; padding:4px 8px;")
    htmltools::tags$table(
      style = "border-collapse:collapse; font-size:14px; margin:8px 0 20px 0;",
      htmltools::tags$thead(htmltools::tags$tr(lapply(names(df), function(n)
        htmltools::tags$th(n, style = "border:1px solid #ccc; padding:4px 8px; background:#eee; text-align:left;")))),
      htmltools::tags$tbody(lapply(seq_len(nrow(df)), function(i)
        htmltools::tags$tr(lapply(df[i, , drop = FALSE], cell)))))
  }
  img <- function(f, cap) {
    if (is.null(f)) return(NULL)
    htmltools::tagList(
      htmltools::tags$img(src = file.path("followup_figures", f), style = "max-width:100%; height:auto;"),
      htmltools::tags$p(cap, style = "color:#555; font-size:13px; margin:2px 0 24px 0;"))
  }
  report <- htmltools::tags$div(
    style = "font-family:sans-serif; max-width:1100px; margin:20px auto; padding:0 16px;",
    htmltools::tags$h1("Follow-up (longitudinal) analysis"),
    htmltools::tags$p(paste0("Generated: ", Sys.time())),
    htmltools::tags$div(
      style = "background:#F1F8F6; border:1px solid #B2DFDB; border-radius:6px; padding:8px 16px; margin:12px 0;",
      htmltools::tags$b("Settings used"),
      htmltools::tags$ul(lapply(settings_lines, htmltools::tags$li))),
    htmltools::tags$p(paste0("This is a longitudinal cohort: every woman was HPV-positive at enrolment and was followed over ", n_full, " visits (baseline plus follow-ups). The figures follow genotype-specific acquisition, persistence and clearance, and summarise each woman's outcome. The definitions of persisted, cleared and acquired, and of the outcome categories, are listed in the settings box above.")),
    htmltools::tags$h2("Figure 1. Participant flow"),
    img(figs$flow, paste0("QC-passed samples. ", length(undated), " women have unreadable visit dates and cannot be placed in a visit order. Percentages are of the ", length(visits_per_woman), " women with a visit order. Women with ", n_full, " visits are analysed; of these, those whose baseline genotype could not be determined or who were not HPV-positive at baseline are set aside before the outcome categories.")),
    
    htmltools::tags$h2("Figure 2. Follow-up outcome of women with complete follow-up (woman level)"),
    img(figs$groups, paste0("Each woman is in exactly one category, so the percentages add up to 100%. ", f2_note, " Not shown, in detail: ", other_text, ".")),
    
    htmltools::tags$h2("Figure 3. Genotypes present at enrolment (the starting pool)"),
    img(figs$pool, paste0("Women by baseline infection: ", base_text, ". Percentages are of women with a typed baseline; a woman with several genotypes is counted once for each.")),
    
    htmltools::tags$h2("Figure 4. Outcome by baseline infection (woman level)"),
    img(figs$outcome, paste0("Women with all ", n_full, " visits and a typed baseline. ", n_notdet_excluded,
                             " woman/women with genotype not determined are not shown.")),
    
    htmltools::tags$h2("Woman-level categories by number of baseline genotypes"),
    htmltools::tags$p("Women with all visits and a typed baseline. The six categories are mutually exclusive, so each woman appears once. Persist + clear can only occur in women with two or more baseline genotypes."),
    tbl(cat_tab),
    htmltools::tags$h2("Summary of 'any' (overlapping)"),
    htmltools::tags$p("A woman can be counted in more than one row (for example a woman who persisted one genotype and cleared another), so these percentages do not add up to 100%."),
    tbl(any_tab),
    
    htmltools::tags$h2("Figure 5. Persist only compared with clear only (woman level)"),
    img(figs$compare, "Women with complete follow-up in the 'persist only' or 'clear only' category. Small groups, so differences are exploratory."),
    htmltools::tags$p("p-values: Fisher's exact test for each characteristic, shown on its first row. Many comparisons on small groups, so treat as exploratory."),
    tbl(compare_show),
    
    htmltools::tags$h2("Figure 6. Persistence and clearance of each baseline genotype (genotype level)"),
    img(figs$fate, "A woman with several baseline genotypes appears once for each, so these observations are not independent (exploratory)."),
    
    htmltools::tags$h2("Figure 7. Newly detected genotypes at follow-up: acquisition (genotype level)"),
    img(figs$acq, "Genotypes with at least one newly detected woman are shown."),
    
    htmltools::tags$h2("Figure 8. Genotype events between consecutive visits (genotype level)"),
    img(figs$trans, "Persisted, cleared and newly detected genotypes at each step of follow-up."),
    
    htmltools::tags$h2("Figure 9. High-risk compared with low-risk genotypes"),
    img(figs$risk, paste0("Genotype-level percentages are exploratory because women contribute several genotypes. ",
                          n_geno_women, " women are included; ", n_geno_missing,
                          " were left out because a follow-up sample had no typed genotype."))
  )
  html_path <- file.path(output_folder, "followup_analysis.html")
  htmltools::save_html(report, html_path)
  
  message("Follow-up database saved: ", xlsx_path)
  message("Follow-up report saved:   ", html_path)
  invisible(list(followup_db = followup_db, visits = visits_tab, figure2 = fig1_tab, groups = grp_tab,
                 outcome_by_baseline = f3, persistence_vs_clearance = compare_tab,
                 category_by_coinfection = cat_tab, any_summary = any_tab,
                 full_followup = three, genotype_fate = fate_tab, acquisition = acq_tab,
                 transitions = trans_tab, risk_rates = risk_rates, prevalence = prev))
}
