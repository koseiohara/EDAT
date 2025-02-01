module EDAT

    use physics      , only : P_E, P_LOG2E, P_LOG10E, P_LN2, P_LN10, P_PI, P_PI_2, P_PI_4, P_1_PI, P_2_PI, &
                            & P_2_SQRTPI, P_SQRT2, P_SQRT1_2                                             , &
                            & GRAV, EarthRadius, GasConstant, Cp, Cv, Lq                                 , &
                            & potential_temperature
    use CaseConverter, only : to_lower, to_upper
    use statistic    , only : corrcoef, covariance, variance, mean
    use BinIO        , only : finfo, fopen, fclose, fread, fwrite, get_record, reset_record

end module EDAT

