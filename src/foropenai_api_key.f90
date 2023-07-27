module foropenai_api_key

   implicit none

   private
   public :: api_key, set_api_key

   character(256) :: api_key

contains

   !===============================================================================
   !> author: Seyed Ali Ghasemi
   function set_api_key(file_name_settings) result(key1)
      character(*), intent(in) :: file_name_settings
      character(256)             :: key1
      integer                  :: nunit, n

      open(newunit=nunit, file=trim(file_name_settings), status="old", action="read", iostat=n)
      if (n /= 0) then
         print*, "error opening the file: ", trim(file_name_settings)
         stop
      end if
      read(nunit, '(a)', iostat=n) key1
      close(nunit)
   end function set_api_key
   !===============================================================================

end module foropenai_api_key
