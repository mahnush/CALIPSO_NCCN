MODULE mo_ham_ccnctl

  USE mo_kind, ONLY: dp

  IMPLICIT NONE

  !--- Prescribed supersaturations from CCN database provided by Dominick Spracklen (pers. comm., 2009)
  !    Included all supersaturations [%] with more than one measurement (31). 

  PRIVATE


  INTEGER,  PUBLIC, PARAMETER :: nsat =6
  REAL(dp), PUBLIC, PARAMETER :: zsat(nsat) = (/0.0010_dp,0.0020_dp,0.0040_dp,0.0060_dp,0.0080_dp,0.0100_dp/)





END MODULE mo_ham_ccnctl
