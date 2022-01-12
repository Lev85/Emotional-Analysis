names(dftotal)[names(dftotal) ==  "fecha"] <-"A_date"
names(dftotal)[names(dftotal) ==  "id"] <-"A_id"
names(dftotal)[names(dftotal) ==  "inicio"] <-"D_start"
names(dftotal)[names(dftotal) ==  "filename"] <-"S_filename"
names(dftotal)[names(dftotal) ==  "weekday"] <-"S_weekday"
names(dftotal)[names(dftotal) ==  "sleeponset_ts"] <-"S_sleeponset_ts"
names(dftotal)[names(dftotal) ==  "wakeup_ts"] <-"S_wakeup_ts"
names(dftotal)[names(dftotal) ==  "guider"] <-"S_guider"
names(dftotal)[names(dftotal) ==  "L5TIME"] <-"S_L5TIME"
names(dftotal)[names(dftotal) ==  "M5TIME"] <-"S_M5TIME"
names(dftotal)[names(dftotal) ==  "L10TIME"] <-"S_L10TIME"
names(dftotal)[names(dftotal) ==  "M10TIME"] <-"S_M10TIME"
names(dftotal)[names(dftotal) ==  "boutdur.in"] <-"S_boutdur.in"
names(dftotal)[names(dftotal) ==  "boutdur.lig"] <-"S_boutdur.lig"
names(dftotal)[names(dftotal) ==  "daytype"] <-"S_daytype"
names(dftotal)[names(dftotal) ==  "Y_dx"] <-"Y_young_dx"
names(dftotal)[names(dftotal) ==  "H_dx"] <-"H_hrsd_dx"
names(dftotal)[names(dftotal) ==  "Informacion"] <-"E_recorded_dx"
names(dftotal)[names(dftotal) ==  "Crisis"] <-"E_crisis"
names(dftotal)[names(dftotal) ==  "dx_largo"] <-"E_dx_large"
names(dftotal)[names(dftotal) ==  "dx_corto"] <-"E_dx_short"
names(dftotal)[names(dftotal) ==  "dx_num"] <-"E_dx_numeric"
names(dftotal)[names(dftotal) ==  "animo"] <-"D_animus"
names(dftotal)[names(dftotal) ==  "ansiedad"] <-"D_anxiety"
names(dftotal)[names(dftotal) ==  "irritabilidad"] <-"D_irritability"
names(dftotal)[names(dftotal) ==  "concentracion"] <-"D_concentration"
names(dftotal)[names(dftotal) ==  "tabaco"] <-"D_tobacco"
names(dftotal)[names(dftotal) ==  "cafeina"] <-"D_caffeine"
names(dftotal)[names(dftotal) ==  "dormir"] <-"D_sleep_time"
names(dftotal)[names(dftotal) ==  "despertar"] <-"D_wakeup_time"
names(dftotal)[names(dftotal) ==  "motivacion"] <-"D_motivation"
names(dftotal)[names(dftotal) ==  "calidad_suenho"] <-"D_sleep_quality"
names(dftotal)[names(dftotal) ==  "lentitud"] <-"D_slowness_fill_form"
names(dftotal)[names(dftotal) ==  "duracionsuenho"] <-"D_sleep_duration"
names(dftotal)[names(dftotal) ==  "cenitsuenho"] <-"D_sleep_zenith"
names(dftotal)[names(dftotal) ==  "alcohol_num"] <-"D_alcohol_consumption"
names(dftotal)[names(dftotal) ==  "otras_drogas_num"] <-"D_drugs_consumption"
names(dftotal)[names(dftotal) ==  "observ_num"] <-"D_observation_string_length"
names(dftotal)[names(dftotal) ==  "horatest"] <-"D_time_start_form"
names(dftotal)[names(dftotal) ==  "lentitudsinoutliers"] <-"D_slowness_fill_form_no_outliers"
names(dftotal)[names(dftotal) ==  "lentitudescalada"] <-"D_slowness_fill_form_no_outliers_scales"
names(dftotal)[names(dftotal) ==  "semana"] <-"S_week"
names(dftotal)[names(dftotal) ==  "diariofill1"] <-"B_formfill1"
names(dftotal)[names(dftotal) ==  "window_number"] <-"S_window_number"
names(dftotal)[names(dftotal) ==  "sleeponset"] <-"S_sleeponset"
names(dftotal)[names(dftotal) ==  "wakeup"] <-"S_wakeup"
names(dftotal)[names(dftotal) ==  "night_number"] <-"S_night_number"
names(dftotal)[names(dftotal) ==  "daysleeper"] <-"S_daysleeper"
names(dftotal)[names(dftotal) ==  "cleaningcode"] <-"S_cleaningcode"
names(dftotal)[names(dftotal) ==  "sleeplog_used"] <-"S_sleeplog_used"
names(dftotal)[names(dftotal) ==  "acc_available"] <-"S_acc_available"
names(dftotal)[names(dftotal) ==  "nonwear_perc_day"] <-"S_nonwear_perc_day"
names(dftotal)[names(dftotal) ==  "nonwear_perc_spt"] <-"S_nonwear_perc_spt"
names(dftotal)[names(dftotal) ==  "nonwear_perc_day_spt"] <-"S_nonwear_perc_day_spt"
names(dftotal)[names(dftotal) ==  "dur_spt_sleep_min"] <-"S_dur_spt_sleep_min"
names(dftotal)[names(dftotal) ==  "dur_spt_wake_IN_min"] <-"S_dur_spt_wake_IN_min"
names(dftotal)[names(dftotal) ==  "dur_spt_wake_LIG_min"] <-"S_dur_spt_wake_LIG_min"
names(dftotal)[names(dftotal) ==  "dur_spt_wake_MOD_min"] <-"S_dur_spt_wake_MOD_min"
names(dftotal)[names(dftotal) ==  "dur_spt_wake_VIG_min"] <-"S_dur_spt_wake_VIG_min"
names(dftotal)[names(dftotal) ==  "dur_day_IN_unbt_min"] <-"S_dur_day_IN_unbt_min"
names(dftotal)[names(dftotal) ==  "dur_day_LIG_unbt_min"] <-"S_dur_day_LIG_unbt_min"
names(dftotal)[names(dftotal) ==  "dur_day_MOD_unbt_min"] <-"S_dur_day_MOD_unbt_min"
names(dftotal)[names(dftotal) ==  "dur_day_VIG_unbt_min"] <-"S_dur_day_VIG_unbt_min"
names(dftotal)[names(dftotal) ==  "dur_day_MVPA_bts_1_min"] <-"S_dur_day_MVPA_bts_1_min"
names(dftotal)[names(dftotal) ==  "dur_day_IN_bts_30_min"] <-"S_dur_day_IN_bts_30_min"
names(dftotal)[names(dftotal) ==  "dur_day_IN_bts_10_30_min"] <-"S_dur_day_IN_bts_10_30_min"
names(dftotal)[names(dftotal) ==  "dur_day_IN_bts_1_10_min"] <-"S_dur_day_IN_bts_1_10_min"
names(dftotal)[names(dftotal) ==  "dur_day_LIG_bts_10_min"] <-"S_dur_day_LIG_bts_10_min"
names(dftotal)[names(dftotal) ==  "dur_day_LIG_bts_1_10_min"] <-"S_dur_day_LIG_bts_1_10_min"
names(dftotal)[names(dftotal) ==  "dur_day_total_IN_min"] <-"S_dur_day_total_IN_min"
names(dftotal)[names(dftotal) ==  "dur_day_total_LIG_min"] <-"S_dur_day_total_LIG_min"
names(dftotal)[names(dftotal) ==  "dur_day_total_MOD_min"] <-"S_dur_day_total_MOD_min"
names(dftotal)[names(dftotal) ==  "dur_day_total_VIG_min"] <-"S_dur_day_total_VIG_min"
names(dftotal)[names(dftotal) ==  "dur_day_min"] <-"S_dur_day_min"
names(dftotal)[names(dftotal) ==  "dur_spt_min"] <-"S_dur_spt_min"
names(dftotal)[names(dftotal) ==  "dur_day_spt_min"] <-"S_dur_day_spt_min"
names(dftotal)[names(dftotal) ==  "N_atleast5minwakenight"] <-"S_N_atleast5minwakenight"
names(dftotal)[names(dftotal) ==  "sleep_efficiency"] <-"S_sleep_efficiency"
names(dftotal)[names(dftotal) ==  "ACC_spt_sleep_mg"] <-"S_ACC_spt_sleep_mg"
names(dftotal)[names(dftotal) ==  "ACC_spt_wake_IN_mg"] <-"S_ACC_spt_wake_IN_mg"
names(dftotal)[names(dftotal) ==  "ACC_spt_wake_LIG_mg"] <-"S_ACC_spt_wake_LIG_mg"
names(dftotal)[names(dftotal) ==  "ACC_spt_wake_MOD_mg"] <-"S_ACC_spt_wake_MOD_mg"
names(dftotal)[names(dftotal) ==  "ACC_spt_wake_VIG_mg"] <-"S_ACC_spt_wake_VIG_mg"
names(dftotal)[names(dftotal) ==  "ACC_day_IN_unbt_mg"] <-"S_ACC_day_IN_unbt_mg"
names(dftotal)[names(dftotal) ==  "ACC_day_LIG_unbt_mg"] <-"S_ACC_day_LIG_unbt_mg"
names(dftotal)[names(dftotal) ==  "ACC_day_MOD_unbt_mg"] <-"S_ACC_day_MOD_unbt_mg"
names(dftotal)[names(dftotal) ==  "ACC_day_VIG_unbt_mg"] <-"S_ACC_day_VIG_unbt_mg"
names(dftotal)[names(dftotal) ==  "ACC_day_MVPA_bts_1_mg"] <-"S_ACC_day_MVPA_bts_1_mg"
names(dftotal)[names(dftotal) ==  "ACC_day_IN_bts_30_mg"] <-"S_ACC_day_IN_bts_30_mg"
names(dftotal)[names(dftotal) ==  "ACC_day_IN_bts_10_30_mg"] <-"S_ACC_day_IN_bts_10_30_mg"
names(dftotal)[names(dftotal) ==  "ACC_day_IN_bts_1_10_mg"] <-"S_ACC_day_IN_bts_1_10_mg"
names(dftotal)[names(dftotal) ==  "ACC_day_LIG_bts_10_mg"] <-"S_ACC_day_LIG_bts_10_mg"
names(dftotal)[names(dftotal) ==  "ACC_day_LIG_bts_1_10_mg"] <-"S_ACC_day_LIG_bts_1_10_mg"
names(dftotal)[names(dftotal) ==  "ACC_day_total_IN_mg"] <-"S_ACC_day_total_IN_mg"
names(dftotal)[names(dftotal) ==  "ACC_day_total_LIG_mg"] <-"S_ACC_day_total_LIG_mg"
names(dftotal)[names(dftotal) ==  "ACC_day_total_MOD_mg"] <-"S_ACC_day_total_MOD_mg"
names(dftotal)[names(dftotal) ==  "ACC_day_total_VIG_mg"] <-"S_ACC_day_total_VIG_mg"
names(dftotal)[names(dftotal) ==  "ACC_day_mg"] <-"S_ACC_day_mg"
names(dftotal)[names(dftotal) ==  "ACC_spt_mg"] <-"S_ACC_spt_mg"
names(dftotal)[names(dftotal) ==  "ACC_day_spt_mg"] <-"S_ACC_day_spt_mg"
names(dftotal)[names(dftotal) ==  "quantile_mostactive60min_mg"] <-"S_quantile_mostactive60min_mg"
names(dftotal)[names(dftotal) ==  "quantile_mostactive30min_mg"] <-"S_quantile_mostactive30min_mg"
names(dftotal)[names(dftotal) ==  "L5VALUE"] <-"S_L5VALUE"
names(dftotal)[names(dftotal) ==  "M5VALUE"] <-"S_M5VALUE"
names(dftotal)[names(dftotal) ==  "L5TIME_num"] <-"S_L5TIME_num"
names(dftotal)[names(dftotal) ==  "M5TIME_num"] <-"S_M5TIME_num"
names(dftotal)[names(dftotal) ==  "L10VALUE"] <-"S_L10VALUE"
names(dftotal)[names(dftotal) ==  "M10VALUE"] <-"S_M10VALUE"
names(dftotal)[names(dftotal) ==  "L10TIME_num"] <-"S_L10TIME_num"
names(dftotal)[names(dftotal) ==  "M10TIME_num"] <-"S_M10TIME_num"
names(dftotal)[names(dftotal) ==  "Nbouts_day_MVPA_bts_1"] <-"S_Nbouts_day_MVPA_bts_1"
names(dftotal)[names(dftotal) ==  "Nbouts_day_IN_bts_30"] <-"S_Nbouts_day_IN_bts_30"
names(dftotal)[names(dftotal) ==  "Nbouts_day_IN_bts_10_30"] <-"S_Nbouts_day_IN_bts_10_30"
names(dftotal)[names(dftotal) ==  "Nbouts_day_IN_bts_1_10"] <-"S_Nbouts_day_IN_bts_1_10"
names(dftotal)[names(dftotal) ==  "Nbouts_day_LIG_bts_10"] <-"S_Nbouts_day_LIG_bts_10"
names(dftotal)[names(dftotal) ==  "Nbouts_day_LIG_bts_1_10"] <-"S_Nbouts_day_LIG_bts_1_10"
names(dftotal)[names(dftotal) ==  "Nblocks_spt_sleep"] <-"S_Nblocks_spt_sleep"
names(dftotal)[names(dftotal) ==  "Nblocks_spt_wake_IN"] <-"S_Nblocks_spt_wake_IN"
names(dftotal)[names(dftotal) ==  "Nblocks_spt_wake_LIG"] <-"S_Nblocks_spt_wake_LIG"
names(dftotal)[names(dftotal) ==  "Nblocks_spt_wake_MOD"] <-"S_Nblocks_spt_wake_MOD"
names(dftotal)[names(dftotal) ==  "Nblocks_spt_wake_VIG"] <-"S_Nblocks_spt_wake_VIG"
names(dftotal)[names(dftotal) ==  "Nblocks_day_IN_unbt"] <-"S_Nblocks_day_IN_unbt"
names(dftotal)[names(dftotal) ==  "Nblocks_day_LIG_unbt"] <-"S_Nblocks_day_LIG_unbt"
names(dftotal)[names(dftotal) ==  "Nblocks_day_MOD_unbt"] <-"S_Nblocks_day_MOD_unbt"
names(dftotal)[names(dftotal) ==  "Nblocks_day_VIG_unbt"] <-"S_Nblocks_day_VIG_unbt"
names(dftotal)[names(dftotal) ==  "Nblocks_day_MVPA_bts_1"] <-"S_Nblocks_day_MVPA_bts_1"
names(dftotal)[names(dftotal) ==  "Nblocks_day_IN_bts_30"] <-"S_Nblocks_day_IN_bts_30"
names(dftotal)[names(dftotal) ==  "Nblocks_day_IN_bts_10_30"] <-"S_Nblocks_day_IN_bts_10_30"
names(dftotal)[names(dftotal) ==  "Nblocks_day_IN_bts_1_10"] <-"S_Nblocks_day_IN_bts_1_10"
names(dftotal)[names(dftotal) ==  "Nblocks_day_LIG_bts_10"] <-"S_Nblocks_day_LIG_bts_10"
names(dftotal)[names(dftotal) ==  "Nblocks_day_LIG_bts_1_10"] <-"S_Nblocks_day_LIG_bts_1_10"
names(dftotal)[names(dftotal) ==  "Nblocks_day_total_IN"] <-"S_Nblocks_day_total_IN"
names(dftotal)[names(dftotal) ==  "Nblocks_day_total_LIG"] <-"S_Nblocks_day_total_LIG"
names(dftotal)[names(dftotal) ==  "Nblocks_day_total_MOD"] <-"S_Nblocks_day_total_MOD"
names(dftotal)[names(dftotal) ==  "Nblocks_day_total_VIG"] <-"S_Nblocks_day_total_VIG"
names(dftotal)[names(dftotal) ==  "boutcriter.in"] <-"S_boutcriter.in"
names(dftotal)[names(dftotal) ==  "boutcriter.lig"] <-"S_boutcriter.lig"
names(dftotal)[names(dftotal) ==  "boutcriter.mvpa"] <-"S_boutcriter.mvpa"
names(dftotal)[names(dftotal) ==  "boutdur.mvpa"] <-"S_boutdur.mvpa"
names(dftotal)[names(dftotal) ==  "bout.metric"] <-"S_bout.metric"
names(dftotal)[names(dftotal) ==  "pulserafill1"] <-"B_smartwatchfill1"
names(dftotal)[names(dftotal) ==  "alivio"] <-"I_alleviation"
names(dftotal)[names(dftotal) ==  "terapeuta"] <-"I_therapist"
names(dftotal)[names(dftotal) ==  "prograda"] <-"I_scheduled_intervention"
names(dftotal)[names(dftotal) ==  "tipo"] <-"I_intervencion_type"
names(dftotal)[names(dftotal) ==  "cambiosignificativo"] <-"I_significant_change"
names(dftotal)[names(dftotal) ==  "I_NAfilled"] <-"I_NAfilled"
names(dftotal)[names(dftotal) ==  "intervencionesfill1"] <-"B_interventionfill1"
names(dftotal)[names(dftotal) ==  "euforia"] <-"Y_euphoria"
names(dftotal)[names(dftotal) ==  "hiperactividad"] <-"Y_hyperactivity"
names(dftotal)[names(dftotal) ==  "impulso_sexual"] <-"Y_sex_drive"
names(dftotal)[names(dftotal) ==  "Suenho"] <-"Y_sleep"
names(dftotal)[names(dftotal) ==  "Irritabilidad"] <-"Y_irritability"
names(dftotal)[names(dftotal) ==  "expresion_verbal"] <-"Y_verbal_expression"
names(dftotal)[names(dftotal) ==  "curso"] <-"Y_thought_disorder"
names(dftotal)[names(dftotal) ==  "contenido"] <-"Y_content"
names(dftotal)[names(dftotal) ==  "Agresividad"] <-"Y_aggressiveness"
names(dftotal)[names(dftotal) ==  "Apariencia"] <-"Y_appearance"
names(dftotal)[names(dftotal) ==  "conciencia.young"] <-"Y_awareness"
names(dftotal)[names(dftotal) ==  "puntaje.young"] <-"Y_score"
names(dftotal)[names(dftotal) ==  "Y_NAfilled"] <-"Y_NAfilled"
names(dftotal)[names(dftotal) ==  "youngfill1"] <-"B_youngfill1"
names(dftotal)[names(dftotal) ==  "depresion"] <-"H_depression"
names(dftotal)[names(dftotal) ==  "culpa"] <-"H_guilt"
names(dftotal)[names(dftotal) ==  "Suicidio"] <-"H_suicide"
names(dftotal)[names(dftotal) ==  "iprecoz"] <-"H_early_insonmia"
names(dftotal)[names(dftotal) ==  "imedio"] <-"H_mid_insonmia"
names(dftotal)[names(dftotal) ==  "itardio"] <-"H_late_insonmia"
names(dftotal)[names(dftotal) ==  "actividad"] <-"H_activity"
names(dftotal)[names(dftotal) ==  "inhibicion"] <-"H_inhibition"
names(dftotal)[names(dftotal) ==  "agitacion"] <-"H_agitation"
names(dftotal)[names(dftotal) ==  "an_psiq"] <-"H_psychic_anxiety"
names(dftotal)[names(dftotal) ==  "an_som"] <-"H_somatic_anxiety"
names(dftotal)[names(dftotal) ==  "gastro"] <-"H_gastrointestinal_somatic_symptoms"
names(dftotal)[names(dftotal) ==  "somatic"] <-"H_general_somatic_symptoms"
names(dftotal)[names(dftotal) ==  "sexo"] <-"H_genitals_symptoms"
names(dftotal)[names(dftotal) ==  "conciencia.hrsd"] <-"H_illness_awareness"
names(dftotal)[names(dftotal) ==  "hipocondria"] <-"H_hypochondria"
names(dftotal)[names(dftotal) ==  "peso"] <-"H_weigth"
names(dftotal)[names(dftotal) ==  "puntaje.hrsd"] <-"H_score"
names(dftotal)[names(dftotal) ==  "H_NAfilled"] <-"H_NAfilled"
names(dftotal)[names(dftotal) ==  "hrsdfill1"] <-"B_hrsdfill1"
names(dftotal)[names(dftotal) ==  "asiste_cita"] <-"I_assist_ind"
names(dftotal)[names(dftotal) ==  "I_dx"] <-"Z_dx_numeric"
names(dftotal)[names(dftotal) ==  "I_dx_char"] <-"I_dx_short"
names(dftotal)[names(dftotal) ==  "I_dx_largo"] <-"I_dx_large"


names(dftotal)[names(dftotal) ==  "D_NAfilled"] <-"B_D_NAfilled"
names(dftotal)[names(dftotal) ==  "S_NAfilled"] <-"B_S_NAfilled"
names(dftotal)[names(dftotal) ==  "I_NAfilled"] <-"B_I_NAfilled"
names(dftotal)[names(dftotal) ==  "H_NAfilled"] <-"B_H_NAfilled"
names(dftotal)[names(dftotal) ==  "Y_NAfilled"] <-"B_Y_NAfilled"





dftotal$E_dx_short<-ifelse(dftotal$E_dx_short=="Eutimia","Euthymia",
						ifelse(dftotal$E_dx_short=="Mixto","Mixed",
								ifelse(dftotal$E_dx_short=="Depresion","Depression",
									ifelse(dftotal$E_dx_short=="Hipomania","Hypomania",
										ifelse(dftotal$E_dx_short=="Ingresado","Hospitalized","Not Informed")
									)
								)
							)
						)
