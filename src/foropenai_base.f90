module foropenai_base

   implicit none

   private
   public :: openai

   !===============================================================================
   !> author: Seyed Ali Ghasemi
   type openai
      character(len=:), allocatable :: organization
      character(len=:), allocatable :: api_key
      character(len=:), allocatable :: file_name
   contains
      procedure :: deallocate_api_key
      procedure :: deallocate_organization
      procedure :: deallocate_file_name
      procedure :: finalize => deallocate_openai
      procedure :: load_api_key
      procedure :: load_organization
      procedure :: load_base_data
      procedure :: print_api_key
      procedure :: print_organization
      procedure :: print_file_name
      procedure :: set_organization
      procedure :: set_api_key
      procedure :: set_file_name
   end type openai
   !===============================================================================

contains

   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine deallocate_openai(this)
      class(openai), intent(inout) :: this
      call this%deallocate_api_key()
      call this%deallocate_organization()
      call this%deallocate_file_name()
   end subroutine deallocate_openai
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine deallocate_api_key(this)
      class(openai), intent(inout) :: this
      if (allocated(this%api_key)) deallocate(this%api_key)
   end subroutine deallocate_api_key
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine deallocate_organization(this)
      class(openai), intent(inout) :: this
      if (allocated(this%organization)) deallocate(this%organization)
   end subroutine deallocate_organization
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine deallocate_file_name(this)
      class(openai), intent(inout) :: this
      if (allocated(this%file_name)) deallocate(this%file_name)
   end subroutine deallocate_file_name
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine load_base_data(this, file_name)
      class(openai),    intent(inout) :: this
      character(len=*), intent(in)    :: file_name
      call this%set_file_name(trim(file_name))
      call this%load_api_key()
      call this%load_organization()
   end subroutine load_base_data
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_organization(this, organization)
      class(openai),    intent(inout) :: this
      character(len=*), intent(in)    :: organization
      this%organization = trim(organization)
   end subroutine set_organization
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_api_key(this, api_key)
      class(openai),    intent(inout) :: this
      character(len=*), intent(in)    :: api_key
      this%api_key = trim(api_key)
   end subroutine set_api_key
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine load_api_key(this)
      use json_module, only: json_file
      class(openai), intent(inout) :: this
      type(json_file)              :: json
      call json%load_file(trim(this%file_name))
      call json%get("base.api_key", this%api_key)
   end subroutine load_api_key
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_file_name(this, file_name)
      class(openai),    intent(inout) :: this
      character(len=*), intent(in)    :: file_name
      this%file_name = trim(file_name)
   end subroutine set_file_name
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine load_organization(this)
      use json_module, only: json_file
      class(openai), intent(inout) :: this
      type(json_file)              :: json
      call json%load_file(trim(this%file_name))
      call json%get("base.organization", this%organization)
   end subroutine load_organization
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine print_api_key(this)
      class(openai), intent(inout) :: this
      print "('api key: ',A)", trim(this%api_key)
   end subroutine print_api_key
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine print_organization(this)
      class(openai), intent(inout) :: this
      print "('organization: ',A)", trim(this%organization)
   end subroutine print_organization
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine print_file_name(this)
      class(openai), intent(inout) :: this
      print "('file name: ',A)", trim(this%file_name)
   end subroutine print_file_name
   !===============================================================================

end module foropenai_base
