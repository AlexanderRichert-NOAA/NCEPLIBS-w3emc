! Unit tests for mersenne_twister module interfaces and state handling.
program test_mersenne_twister
  use mersenne_twister
  implicit none

  call test_seed_state_roundtrip()
  call test_interface_consistency()
  call test_gauss_cache_and_index_bounds()

  print *, 'SUCCESS!'

contains

  subroutine test_seed_state_roundtrip()
    type(random_stat) :: s1, s2
    integer :: nseed
    integer, allocatable :: seed(:), bad_seed(:)
    real :: h1(16), h2(16)

    call random_seed(size=nseed)
    call expect_true(nseed > 100, 'seed size should be reported')
    allocate(seed(nseed), bad_seed(nseed))

    call random_setseed(12345, s1)
    call random_seed(get=seed, stat=s1)
    call random_number(h1, s1)

    call random_seed(put=seed, stat=s2)
    call random_number(h2, s2)
    call expect_real_array_equal(h1, h2, 'restored state must reproduce sequence')

    bad_seed = seed
    bad_seed(1) = -1
    call random_seed(put=bad_seed, stat=s1)

    call random_seed(stat=s2)
    call random_number(h1, s1)
    call random_number(h2, s2)
    call expect_real_array_equal(h1, h2, 'invalid put should fall back to default seed')

    call random_setseed(0, s1)
    call random_seed(stat=s2)
    call random_number(h1, s1)
    call random_number(h2, s2)
    call expect_real_array_equal(h1, h2, 'seed=0 should map to default seed')

    call random_setseed(86420)
    call random_seed(get=seed)
    call random_number(h1)

    call random_seed(put=seed)
    call random_number(h2)
    call expect_real_array_equal(h1, h2, 'saved-mode put without stat must restore sequence')

    bad_seed = seed
    bad_seed(1) = -1
    call random_seed(put=bad_seed)
    call random_number(h1)

    call random_seed()
    call random_number(h2)
    call expect_real_array_equal(h1, h2, 'saved-mode invalid put should reset to default seed')

    deallocate(seed, bad_seed)
  end subroutine test_seed_state_roundtrip

  subroutine test_interface_consistency()
    type(random_stat) :: st
    real :: from_i(8), from_t(8), batch(1), func_value
    integer :: idx_i(8), idx_t(12), idx_s(12)
    integer :: ifunc
    logical :: has_low, has_high

    call random_number(from_i, 777)
    call random_setseed(777, st)
    call random_number(from_t, st)
    call expect_real_array_equal(from_i, from_t, 'random_number interactive must match thread-safe')

    call random_gauss(from_i, 777)
    call random_setseed(777, st)
    call random_gauss(from_t, st)
    call expect_real_array_equal(from_i, from_t, 'random_gauss interactive must match thread-safe')

    call random_index(17, idx_i, 777)
    call random_setseed(777, st)
    call random_index(17, idx_t(1:8), st)
    call expect_int_array_equal(idx_i, idx_t(1:8), 'random_index interactive must match thread-safe')

    call random_seed()
    call random_index(19, idx_s)
    call random_seed(stat=st)
    call random_index(19, idx_t, st)
    call expect_int_array_equal(idx_s, idx_t, 'random_index saved mode must match thread-safe')

    call random_seed()
    ifunc = random_index_f(19)
    call random_seed()
    call random_index(19, idx_s(1:1))
    call expect_true(ifunc == idx_s(1), 'random_index_f should match saved-mode first value')
    call expect_true(ifunc >= 1 .and. ifunc <= 19, 'random_index_f must stay in [1,imax]')

    call random_seed()
    func_value = random_number_f()
    call random_seed()
    call random_number(batch)
    call expect_true(func_value == batch(1), 'random_number_f should match saved-mode first value')

    call random_seed()
    func_value = random_gauss_f()
    call random_seed()
    call random_gauss(batch)
    call expect_true(func_value == batch(1), 'random_gauss_f should match saved-mode first value')

    call random_seed()
    has_low = .false.
    has_high = .false.
    call random_number(from_i)
    has_low = any(from_i < 0.5)
    has_high = any(from_i > 0.5)
    call random_number(from_t)
    has_low = has_low .or. any(from_t < 0.5)
    has_high = has_high .or. any(from_t > 0.5)
    call expect_true(has_low .and. has_high, 'sequence should include values on both sides of 0.5')
  end subroutine test_interface_consistency

  subroutine test_gauss_cache_and_index_bounds()
    type(random_stat) :: s_pair, s_split
    real :: pair(2), first(1), second(1)
    real :: empty(0)
    integer :: idx(700)

    call random_setseed(2468, s_pair)
    call random_setseed(2468, s_split)

    call random_gauss(pair, s_pair)
    call random_gauss(first, s_split)
    call random_gauss(second, s_split)

    call expect_true(first(1) == pair(1), 'first Gaussian value should match pair generation')
    call expect_true(second(1) == pair(2), 'cached Gaussian value should be reused on next call')

    call random_gauss(empty, s_split)

    call random_setseed(11, s_split)
    call random_index(17, idx, s_split)
    call expect_true(all(idx >= 1) .and. all(idx <= 17), 'random_index values must stay in [1,imax]')
  end subroutine test_gauss_cache_and_index_bounds

  subroutine expect_true(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) then
      print *, 'FAIL: ', trim(message)
      stop 1
    end if
  end subroutine expect_true

  subroutine expect_real_array_equal(a, b, message)
    real, intent(in) :: a(:), b(:)
    character(len=*), intent(in) :: message

    if (size(a) /= size(b)) then
      print *, 'FAIL: ', trim(message), ' size mismatch'
      stop 2
    end if
    if (any(a /= b)) then
      print *, 'FAIL: ', trim(message)
      stop 3
    end if
  end subroutine expect_real_array_equal

  subroutine expect_int_array_equal(a, b, message)
    integer, intent(in) :: a(:), b(:)
    character(len=*), intent(in) :: message

    if (size(a) /= size(b)) then
      print *, 'FAIL: ', trim(message), ' size mismatch'
      stop 4
    end if
    if (any(a /= b)) then
      print *, 'FAIL: ', trim(message)
      stop 5
    end if
  end subroutine expect_int_array_equal

end program test_mersenne_twister
