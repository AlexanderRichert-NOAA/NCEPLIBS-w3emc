! Unit tests for r63w72() conversion of w3fi63-style KPDS/KGDS to w3fi72 IPDS/IGDS.
program test_r63w72
  implicit none

  call test_latlon_with_pl_extension()
  call test_mercator_and_extended_pds()
  call test_lambert_exception()
  call test_default_grid_path()

  print *, 'SUCCESS!'

contains

  subroutine test_latlon_with_pl_extension()
    integer :: kpds(200), kgds(200), ipds(200), igds(200)

    call fill_defaults(kpds, kgds, ipds, igds)

    kpds(1) = 7
    kpds(2) = 2
    kpds(3) = 255
    kpds(4) = 192
    kpds(5) = 11
    kpds(6) = 101
    kpds(7) = 513
    kpds(8) = 24
    kpds(9) = 5
    kpds(10) = 17
    kpds(11) = 6
    kpds(12) = 30
    kpds(13) = 1
    kpds(14) = 12
    kpds(15) = 18
    kpds(16) = 10
    kpds(17) = 4
    kpds(19) = 2
    kpds(20) = 3
    kpds(21) = 21
    kpds(22) = -5
    kpds(23) = 1

    kgds(1) = 0
    kgds(2) = 73
    kgds(3) = 3
    kgds(4) = 40
    kgds(5) = 50
    kgds(6) = 60
    kgds(7) = 70
    kgds(8) = 80
    kgds(9) = 900
    kgds(10) = 1000
    kgds(11) = 110
    kgds(12) = 120
    kgds(13) = 130
    kgds(14) = 140
    kgds(15) = 150
    kgds(16) = 160
    kgds(19) = 0
    kgds(20) = 10
    kgds(22) = 501
    kgds(23) = 502
    kgds(24) = 503

    call r63w72(kpds, kgds, ipds, igds)

    call expect_eq_int(ipds(1), 28, 'latlon: ipds(1) normal length')
    call expect_eq_int(ipds(2), 2, 'latlon: ipds(2) table version')
    call expect_eq_int(ipds(6), 1, 'latlon: ipds(6) gds flag')
    call expect_eq_int(ipds(7), 1, 'latlon: ipds(7) bms flag')
    call expect_eq_int(ipds(10), 2, 'latlon: split level hi byte')
    call expect_eq_int(ipds(11), 1, 'latlon: split level lo byte')
    call expect_eq_int(ipds(25), -5, 'latlon: decimal scaling copy')
    call expect_eq_int(ipds(26), -999, 'latlon: ipds(26) untouched for 28-byte pds')

    call expect_eq_int(igds(1), 0, 'latlon: igds(1) nv')
    call expect_eq_int(igds(2), 10, 'latlon: igds(2) pv/pl indicator')
    call expect_eq_int(igds(3), 0, 'latlon: igds(3) representation')
    call expect_eq_int(igds(4), 73, 'latlon: igds(4) copy')
    call expect_eq_int(igds(11), 1000, 'latlon: exception igds(11)=kgds(10)')
    call expect_eq_int(igds(12), 900, 'latlon: exception igds(12)=kgds(9)')
    call expect_eq_int(igds(19), 501, 'latlon: pl extension first value')
    call expect_eq_int(igds(20), 502, 'latlon: pl extension second value')
    call expect_eq_int(igds(21), 503, 'latlon: pl extension third value')
  end subroutine test_latlon_with_pl_extension

  subroutine test_mercator_and_extended_pds()
    integer :: kpds(200), kgds(200), ipds(200), igds(200)

    call fill_defaults(kpds, kgds, ipds, igds)

    kpds(4) = 64
    kpds(6) = 100
    kpds(7) = 777
    kpds(23) = 2

    kgds(1) = 1
    kgds(9) = 901
    kgds(10) = 1001
    kgds(11) = 1111
    kgds(12) = 1212
    kgds(13) = 1313
    kgds(19) = 2
    kgds(20) = 255

    call r63w72(kpds, kgds, ipds, igds)

    call expect_eq_int(ipds(1), 45, 'mercator: ipds(1) extended length')
    call expect_eq_int(ipds(6), 0, 'mercator: ipds(6) gds flag')
    call expect_eq_int(ipds(7), 1, 'mercator: ipds(7) bms flag')
    call expect_eq_int(ipds(10), 0, 'mercator: unsplit level hi byte')
    call expect_eq_int(ipds(11), 777, 'mercator: unsplit level value')
    call expect_eq_int(ipds(26), 0, 'mercator: extended pds byte 29')
    call expect_eq_int(ipds(27), 0, 'mercator: extended pds byte 30')

    call expect_eq_int(igds(3), 1, 'mercator: representation')
    call expect_eq_int(igds(11), 1313, 'mercator: exception igds(11)=kgds(13)')
    call expect_eq_int(igds(12), 1212, 'mercator: exception igds(12)=kgds(12)')
    call expect_eq_int(igds(13), 901, 'mercator: exception igds(13)=kgds(9)')
    call expect_eq_int(igds(14), 1111, 'mercator: exception igds(14)=kgds(11)')
  end subroutine test_mercator_and_extended_pds

  subroutine test_lambert_exception()
    integer :: kpds(200), kgds(200), ipds(200), igds(200)

    call fill_defaults(kpds, kgds, ipds, igds)

    kpds(6) = 236
    kpds(7) = 258

    kgds(1) = 3
    kgds(12) = 212
    kgds(13) = 213
    kgds(14) = 214
    kgds(15) = 215

    call r63w72(kpds, kgds, ipds, igds)

    call expect_eq_int(ipds(10), 1, 'lambert: split level hi byte')
    call expect_eq_int(ipds(11), 2, 'lambert: split level lo byte')

    call expect_eq_int(igds(3), 3, 'lambert: representation')
    call expect_eq_int(igds(15), 212, 'lambert: exception igds(15)=kgds(12)')
    call expect_eq_int(igds(16), 213, 'lambert: exception igds(16)=kgds(13)')
    call expect_eq_int(igds(17), 214, 'lambert: exception igds(17)=kgds(14)')
    call expect_eq_int(igds(18), 215, 'lambert: exception igds(18)=kgds(15)')
  end subroutine test_lambert_exception

  subroutine test_default_grid_path()
    integer :: kpds(200), kgds(200), ipds(200), igds(200)

    call fill_defaults(kpds, kgds, ipds, igds)

    kpds(6) = 10
    kpds(7) = 42

    kgds(1) = 2
    kgds(9) = 909
    kgds(10) = 1010
    kgds(13) = 1310
    kgds(14) = 1410
    kgds(15) = 1510
    kgds(16) = 1610
    kgds(19) = 1
    kgds(20) = 255

    call r63w72(kpds, kgds, ipds, igds)

    call expect_eq_int(ipds(10), 0, 'default: unsplit level hi byte')
    call expect_eq_int(ipds(11), 42, 'default: unsplit level value')

    call expect_eq_int(igds(3), 2, 'default: representation')
    call expect_eq_int(igds(11), 909, 'default: igds(11) unchanged copy from kgds(9)')
    call expect_eq_int(igds(12), 1010, 'default: igds(12) unchanged copy from kgds(10)')
    call expect_eq_int(igds(15), 1310, 'default: igds(15) unchanged copy from kgds(13)')
    call expect_eq_int(igds(16), 1410, 'default: igds(16) unchanged copy from kgds(14)')
    call expect_eq_int(igds(17), 1510, 'default: igds(17) unchanged copy from kgds(15)')
    call expect_eq_int(igds(18), 1610, 'default: igds(18) unchanged copy from kgds(16)')
    call expect_eq_int(igds(19), -999, 'default: no pl extension when kgds(1) is not 0')
  end subroutine test_default_grid_path

  subroutine fill_defaults(kpds, kgds, ipds, igds)
    integer, intent(out) :: kpds(200), kgds(200), ipds(200), igds(200)

    kpds = 0
    kgds = 0
    ipds = -999
    igds = -999
  end subroutine fill_defaults

  subroutine expect_eq_int(actual, expected, message)
    integer, intent(in) :: actual, expected
    character(len=*), intent(in) :: message

    if (actual /= expected) then
      print *, 'FAIL: ', trim(message), ' expected=', expected, ' actual=', actual
      stop 1
    end if
  end subroutine expect_eq_int

end program test_r63w72