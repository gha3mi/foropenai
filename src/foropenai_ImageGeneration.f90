module foropenai_ImageGeneration

   use foropenai_base

   implicit none

   private
   public :: ImageGeneration

   !===============================================================================
   !> author: Seyed Ali Ghasemi
   type, extends(openai) :: ImageGeneration
      character(len=:),    allocatable :: url
      character(len=9)                 :: size='1024x1024'
      character(len=1000)              :: prompt
      character(len=4)                 :: response_format='url'
      integer                          :: n=1
      character(len=:),    allocatable :: user_name
      character(len=1024), allocatable :: assistant_response(:)
   contains
      procedure :: create => create_image
      procedure, private :: deallocate_url
      procedure, private :: deallocate_user_name
      procedure, private :: deallocate_assistant_response
      procedure :: finalize => deallocate_ImageGeneration
      procedure, private :: load_url
      procedure, private :: load_size
      procedure, private :: load_response_format
      procedure, private :: load_n
      procedure, private :: load_user_name
      procedure, private :: load => load_ImageGeneration_data
      procedure, private :: print_url
      procedure, private :: print_size
      procedure, private :: print_prompt
      procedure, private :: print_response_format
      procedure, private :: print_n
      procedure, private :: print_user_name
      procedure :: print_assistant_response
      procedure, private :: set_url
      procedure, private :: set_size
      procedure, private :: set_prompt
      procedure, private :: set_response_format
      procedure, private :: set_n
      procedure, private :: set_user_name
      procedure, private :: set_assistant_response
      procedure :: set => set_ImageGeneration_data
   end type ImageGeneration
   !===============================================================================

contains

   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine create_image(this, prompt, n, size, response_format, user_name)
      use http,        only: response_type, request, HTTP_POST, pair_type
      use json_module, only: json_file

      class(ImageGeneration), intent(inout)        :: this
      character(len=*),       intent(in)           :: prompt
      integer,                intent(in), optional :: n
      character(len=*),       intent(in), optional :: size
      character(len=*),       intent(in), optional :: response_format
      character(len=*),       intent(in), optional :: user_name
      character(len=:),       allocatable          :: assistant_response
      character(len=:),       allocatable          :: jsonstr
      type(pair_type),        allocatable          :: req_header(:)
      type(response_type)                          :: response
      type(json_file)                              :: json
      logical                                      :: found
      integer                                      :: i
      character(len=10)                            :: i_str

      call this%set_prompt(prompt=prompt)
      if (present(n)) call this%set_n(n=n)
      if (present(size)) call this%set_size(size=size)
      if (present(response_format)) call this%set_response_format(response_format=response_format)
      if (present(user_name)) call this%set_user_name(user_name=user_name)

      req_header = [&
         pair_type('Content-Type', 'application/json'),&
         pair_type('Authorization', 'Bearer '//trim(this%api_key)//''),&
         pair_type('OpenAI-Organization', ' '//trim(this%organization)//'')&
         ]

      call json%initialize()
      call json%add('prompt', trim(this%prompt))
      call json%add('n', this%n)
      call json%add('size', trim(this%size))
      call json%add('response_format', trim(this%response_format))
      call json%add('user', trim(this%user_name))
      call json%print_to_string(jsonstr)
      call json%destroy()

      response = request(url=trim(this%url), method=HTTP_POST, data=jsonstr, header=req_header)

      if (response%ok) then
         allocate(this%assistant_response(this%n))
         call json%initialize()
         call json%deserialize(response%content)         
         do i = 1, this%n
            write (i_str, "(I10)") i
            call json%get("data("//trim(i_str)//").url", assistant_response, found=found)
            call this%set_assistant_response(assistant_response=assistant_response, i=i)
         end do
         call json%destroy()
      else
         print '(A)', 'Sorry, an error occurred while processing your request.'
         print '(A)', 'Error message:', response%err_msg
      end if

   end subroutine create_image
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine load_url(this)
      use json_module, only: json_file
      class(ImageGeneration), intent(inout) :: this
      type(json_file)                      :: json
      character(len=:), allocatable        :: tmp
      logical                              :: found
      call json%initialize()
      call json%load_file(trim(this%file_name))
      call json%get("ImageGeneration.url", tmp, found=found)
      if (found) this%url = trim(tmp)
      call json%destroy()
   end subroutine load_url
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine load_size(this)
      use json_module, only: json_file
      class(ImageGeneration), intent(inout) :: this
      type(json_file)                      :: json
      character(len=:), allocatable        :: tmp
      logical                              :: found
      call json%initialize()
      call json%load_file(trim(this%file_name))
      call json%get("ImageGeneration.size", tmp, found=found)
      if (found) this%size = trim(tmp)
      call json%destroy()
   end subroutine load_size
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine load_response_format(this)
      use json_module, only: json_file
      class(ImageGeneration), intent(inout) :: this
      type(json_file)                      :: json
      character(len=:), allocatable        :: tmp
      logical                              :: found
      call json%initialize()
      call json%load_file(trim(this%file_name))
      call json%get("ImageGeneration.response_format", tmp, found=found)
      if (found) this%response_format = trim(tmp)
      call json%destroy()
   end subroutine load_response_format
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine load_n(this)
      use json_module, only: json_file
      class(ImageGeneration), intent(inout) :: this
      type(json_file)                      :: json
      integer                              :: tmp
      logical                              :: found
      call json%initialize()
      call json%load_file(trim(this%file_name))
      call json%get("ImageGeneration.n", tmp, found=found)
      if (found) this%n = tmp
      call json%destroy()
   end subroutine load_n
   !================================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine load_user_name(this)
      use json_module, only: json_file
      class(ImageGeneration), intent(inout) :: this
      type(json_file)                      :: json
      character(len=:), allocatable        :: tmp
      logical                              :: found
      call json%initialize()
      call json%load_file(trim(this%file_name))
      call json%get("ImageGeneration.user_name", tmp, found=found)
      if (found) this%user_name = trim(tmp)
      call json%destroy()
   end subroutine load_user_name
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine print_url(this)
      class(ImageGeneration), intent(in) :: this
      print *, "url: ", this%url
   end subroutine print_url
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine print_size(this)
      class(ImageGeneration), intent(in) :: this
      print *, "size: ", this%size
   end subroutine print_size
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine print_prompt(this)
      class(ImageGeneration), intent(in) :: this
      print *, "prompt: ", this%prompt
   end subroutine print_prompt
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine print_response_format(this)
      class(ImageGeneration), intent(in) :: this
      print *, "response_format: ", this%response_format
   end subroutine print_response_format
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine print_n(this)
      class(ImageGeneration), intent(in) :: this
      print *, "n: ", this%n
   end subroutine print_n
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine print_user_name(this)
      class(ImageGeneration), intent(in) :: this
      print *, "user_name: ", this%user_name
   end subroutine print_user_name
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine print_assistant_response(this)
      class(ImageGeneration), intent(in) :: this
      integer                            :: i
      do i = 1, this%n
         print '(a,g0,a,a)', "assistant_response(", i, "): ", trim(this%assistant_response(i))
      end do
   end subroutine print_assistant_response
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_url(this, url)
      class(ImageGeneration), intent(inout) :: this
      character(len=*),       intent(in)    :: url
      this%url = trim(url)
   end subroutine set_url
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_size(this, size)
      class(ImageGeneration), intent(inout) :: this
      character(len=*),       intent(in)    :: size
      this%size = trim(size)
   end subroutine set_size
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_prompt(this, prompt)
      class(ImageGeneration), intent(inout) :: this
      character(len=*),       intent(in)    :: prompt
      this%prompt = trim(prompt)
   end subroutine set_prompt
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_response_format(this, response_format)
      class(ImageGeneration), intent(inout) :: this
      character(len=*),       intent(in)    :: response_format
      this%response_format = trim(response_format)
   end subroutine set_response_format
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_n(this, n)
      class(ImageGeneration), intent(inout) :: this
      integer,                intent(in)    :: n
      this%n = n
   end subroutine set_n
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_user_name(this, user_name)
      class(ImageGeneration), intent(inout) :: this
      character(len=*),       intent(in)    :: user_name
      this%user_name = trim(user_name)
   end subroutine set_user_name
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_assistant_response(this, assistant_response, i)
      class(ImageGeneration), intent(inout) :: this
      character(len=*),       intent(in)    :: assistant_response
      integer,                intent(in)    :: i
      this%assistant_response(i) = trim(assistant_response)
   end subroutine set_assistant_response
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine set_ImageGeneration_data(this, file_name, &
      url, size, prompt, response_format, n, user_name)
      class(ImageGeneration),    intent(inout) :: this
      character(len=*), optional, intent(in)    :: file_name
      character(len=*), optional, intent(in)    :: url
      character(len=*), optional, intent(in)    :: size
      character(len=*), optional, intent(in)    :: prompt
      character(len=*), optional, intent(in)    :: response_format
      integer,          optional, intent(in)    :: n
      character(len=*), optional, intent(in)    :: user_name

      if (present(url)) call this%set_url(url=url)
      if (present(size)) call this%set_size(size=size)
      if (present(prompt)) call this%set_prompt(prompt=prompt)
      if (present(response_format)) call this%set_response_format(response_format=response_format)
      if (present(n)) call this%set_n(n=n)
      if (present(user_name)) call this%set_user_name(user_name=user_name)

      if (present(file_name)) then
         call this%set_file_name(file_name)
         call this%load(file_name)
      end if
   end subroutine set_ImageGeneration_data
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine load_ImageGeneration_data(this, file_name)
      class(ImageGeneration), intent(inout) :: this
      character(len=*),      intent(in)    :: file_name
      call this%set_file_name(trim(file_name))
      call this%load_url()
      call this%load_size()
      call this%load_response_format()
      call this%load_n()
      call this%load_user_name()
   end subroutine load_ImageGeneration_data
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine deallocate_url(this)
      class(ImageGeneration), intent(inout) :: this
      if (allocated(this%url)) deallocate(this%url)
   end subroutine deallocate_url
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine deallocate_user_name(this)
      class(ImageGeneration), intent(inout) :: this
      if (allocated(this%user_name)) deallocate(this%user_name)
   end subroutine deallocate_user_name
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine deallocate_assistant_response(this)
      class(ImageGeneration), intent(inout) :: this
      if (allocated(this%assistant_response)) deallocate(this%assistant_response)
   end subroutine deallocate_assistant_response
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine deallocate_ImageGeneration(this)
      class(ImageGeneration), intent(inout) :: this
      call this%deallocate_url()
      call this%deallocate_user_name()
      call this%deallocate_assistant_response()
   end subroutine deallocate_ImageGeneration
   !===============================================================================

end module foropenai_ImageGeneration
