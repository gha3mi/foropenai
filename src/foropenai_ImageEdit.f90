module foropenai_ImageEdit

   use foropenai_base

   implicit none

   private
   public :: ImageEdit

   !===============================================================================
   !> author: Seyed Ali Ghasemi
   type, extends(openai) :: ImageEdit
      character(len=:),    allocatable :: url
      character(len=:),    allocatable :: image
      character(len=:),    allocatable :: mask
      character(len=9)                 :: size='1024x1024'
      character(len=1000)              :: prompt
      character(len=4)                 :: response_format='url'
      integer                          :: n=1
      character(len=:),    allocatable :: user_name
      character(len=1024), allocatable :: assistant_response(:)
   contains
      procedure :: create => create_image_edit
      procedure :: deallocate_url
      procedure :: deallocate_user_name
      procedure :: deallocate_assistant_response
      procedure :: deallocate_image
      procedure :: deallocate_mask
      procedure :: finalize => deallocate_ImageEdit
      procedure :: load_url
      procedure :: load_size
      procedure :: load_response_format
      procedure :: load_n
      procedure :: load_user_name
      procedure :: load => load_ImageEdit_data
      procedure :: print_url
      procedure :: print_size
      procedure :: print_prompt
      procedure :: print_response_format
      procedure :: print_n
      procedure :: print_user_name
      procedure :: print_assistant_response
      procedure :: print_image
      procedure :: print_mask
      procedure :: set_url
      procedure :: set_size
      procedure :: set_prompt
      procedure :: set_response_format
      procedure :: set_n
      procedure :: set_user_name
      procedure :: set_assistant_response
      procedure :: set_image
      procedure :: set_mask
      procedure :: set => set_ImageEdit_data
   end type ImageEdit
   !===============================================================================

contains

   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine create_image_edit(this, image, mask, prompt, n, size, response_format, user_name)
      use http,        only: response_type, request, HTTP_POST, pair_type
      use json_module, only: json_file

      class(ImageEdit),       intent(inout)        :: this
      character(len=*),       intent(in)           :: image
      character(len=*),       intent(in)           :: prompt
      character(len=*),       intent(in), optional :: mask
      integer,                intent(in), optional :: n
      character(len=*),       intent(in), optional :: size
      character(len=*),       intent(in), optional :: response_format
      character(len=*),       intent(in), optional :: user_name
      character(len=:),       allocatable          :: assistant_response
      character(len=:),       allocatable          :: jsonstr
      type(pair_type),        allocatable          :: req_header(:), form_data(:), file_data
      type(response_type)                          :: response
      type(json_file)                              :: json
      logical                                      :: found
      integer                                      :: i
      character(len=10)                            :: i_str
      character(len=10)                          :: n_str

      call this%set_prompt(prompt=prompt)
      if (present(n)) call this%set_n(n=n)
      if (present(size)) call this%set_size(size=size)
      if (present(response_format)) call this%set_response_format(response_format=response_format)
      if (present(user_name)) call this%set_user_name(user_name=user_name)

      req_header = [&
         pair_type('Content-Type', 'multipart/form-data'),&
         pair_type('Authorization', 'Bearer '//trim(this%api_key)),&
         pair_type('OpenAI-Organization', ' '//trim(this%organization))&
         ]

      write(n_str, "(I10)") this%n

      form_data = [&
         pair_type('size', trim(this%size)),&
         pair_type('prompt', trim(this%prompt)),&
         pair_type('response_format', trim(this%response_format)),&
         pair_type('n', trim(n_str)),&
         pair_type('user', trim(this%user_name))&
         ]

      file_data =  pair_type('image', trim(this%image))

      response = request(url=trim(this%url), method=HTTP_POST, header=req_header, form=form_data, file=file_data)

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

   end subroutine create_image_edit
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine load_url(this)
      use json_module, only: json_file
      class(ImageEdit), intent(inout) :: this
      type(json_file)                      :: json
      character(len=:), allocatable        :: tmp
      logical                              :: found
      call json%initialize()
      call json%load_file(trim(this%file_name))
      call json%get("ImageEdit.url", tmp, found=found)
      if (found) this%url = trim(tmp)
      call json%destroy()
   end subroutine load_url
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine load_size(this)
      use json_module, only: json_file
      class(ImageEdit), intent(inout) :: this
      type(json_file)                      :: json
      character(len=:), allocatable        :: tmp
      logical                              :: found
      call json%initialize()
      call json%load_file(trim(this%file_name))
      call json%get("ImageEdit.size", tmp, found=found)
      if (found) this%size = trim(tmp)
      call json%destroy()
   end subroutine load_size
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine load_response_format(this)
      use json_module, only: json_file
      class(ImageEdit), intent(inout) :: this
      type(json_file)                      :: json
      character(len=:), allocatable        :: tmp
      logical                              :: found
      call json%initialize()
      call json%load_file(trim(this%file_name))
      call json%get("ImageEdit.response_format", tmp, found=found)
      if (found) this%response_format = trim(tmp)
      call json%destroy()
   end subroutine load_response_format
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine load_n(this)
      use json_module, only: json_file
      class(ImageEdit), intent(inout) :: this
      type(json_file)                      :: json
      integer                              :: tmp
      logical                              :: found
      call json%initialize()
      call json%load_file(trim(this%file_name))
      call json%get("ImageEdit.n", tmp, found=found)
      if (found) this%n = tmp
      call json%destroy()
   end subroutine load_n
   !================================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine load_user_name(this)
      use json_module, only: json_file
      class(ImageEdit), intent(inout) :: this
      type(json_file)                      :: json
      character(len=:), allocatable        :: tmp
      logical                              :: found
      call json%initialize()
      call json%load_file(trim(this%file_name))
      call json%get("ImageEdit.user_name", tmp, found=found)
      if (found) this%user_name = trim(tmp)
      call json%destroy()
   end subroutine load_user_name
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine print_image(this)
      class(ImageEdit), intent(in) :: this
      print *, "image: ", this%image
   end subroutine print_image
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine print_mask(this)
      class(ImageEdit), intent(in) :: this
      print *, "mask: ", this%mask
   end subroutine print_mask
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine print_url(this)
      class(ImageEdit), intent(in) :: this
      print *, "url: ", this%url
   end subroutine print_url
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine print_size(this)
      class(ImageEdit), intent(in) :: this
      print *, "size: ", this%size
   end subroutine print_size
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine print_prompt(this)
      class(ImageEdit), intent(in) :: this
      print *, "prompt: ", this%prompt
   end subroutine print_prompt
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine print_response_format(this)
      class(ImageEdit), intent(in) :: this
      print *, "response_format: ", this%response_format
   end subroutine print_response_format
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine print_n(this)
      class(ImageEdit), intent(in) :: this
      print *, "n: ", this%n
   end subroutine print_n
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine print_user_name(this)
      class(ImageEdit), intent(in) :: this
      print *, "user_name: ", this%user_name
   end subroutine print_user_name
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine print_assistant_response(this)
      class(ImageEdit), intent(in) :: this
      integer                            :: i
      do i = 1, this%n
         print '(a,g0,a,a)', "assistant_response(", i, "): ", trim(this%assistant_response(i))
      end do
   end subroutine print_assistant_response
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_image(this, image)
      class(ImageEdit), intent(inout) :: this
      character(len=*), intent(in)    :: image
      this%image = trim(image)
   end subroutine set_image
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_mask(this, mask)
      class(ImageEdit), intent(inout) :: this
      character(len=*), intent(in)    :: mask
      this%mask = trim(mask)
   end subroutine set_mask
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_url(this, url)
      class(ImageEdit), intent(inout) :: this
      character(len=*),       intent(in)    :: url
      this%url = trim(url)
   end subroutine set_url
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_size(this, size)
      class(ImageEdit), intent(inout) :: this
      character(len=*),       intent(in)    :: size
      this%size = trim(size)
   end subroutine set_size
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_prompt(this, prompt)
      class(ImageEdit), intent(inout) :: this
      character(len=*),       intent(in)    :: prompt
      this%prompt = trim(prompt)
   end subroutine set_prompt
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_response_format(this, response_format)
      class(ImageEdit), intent(inout) :: this
      character(len=*),       intent(in)    :: response_format
      this%response_format = trim(response_format)
   end subroutine set_response_format
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_n(this, n)
      class(ImageEdit), intent(inout) :: this
      integer,                intent(in)    :: n
      this%n = n
   end subroutine set_n
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_user_name(this, user_name)
      class(ImageEdit), intent(inout) :: this
      character(len=*),       intent(in)    :: user_name
      this%user_name = trim(user_name)
   end subroutine set_user_name
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_assistant_response(this, assistant_response, i)
      class(ImageEdit), intent(inout) :: this
      character(len=*),       intent(in)    :: assistant_response
      integer,                intent(in)    :: i
      this%assistant_response(i) = trim(assistant_response)
   end subroutine set_assistant_response
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine set_ImageEdit_data(this, file_name, &
      url, size, response_format, n, user_name)
      class(ImageEdit),           intent(inout) :: this
      character(len=*), optional, intent(in)    :: file_name
      character(len=*), optional, intent(in)    :: url
      character(len=*), optional, intent(in)    :: size
      character(len=*), optional, intent(in)    :: response_format
      integer,          optional, intent(in)    :: n
      character(len=*), optional, intent(in)    :: user_name
      
      if (present(url)) call this%set_url(url=url)
      if (present(size)) call this%set_size(size=size)
      if (present(response_format)) call this%set_response_format(response_format=response_format)
      if (present(n)) call this%set_n(n=n)
      if (present(user_name)) call this%set_user_name(user_name=user_name)

      if (present(file_name)) then
         call this%set_file_name(file_name)
         call this%load(file_name)
      end if
   end subroutine set_ImageEdit_data
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine load_ImageEdit_data(this, file_name)
      class(ImageEdit), intent(inout) :: this
      character(len=*),      intent(in)    :: file_name
      call this%set_file_name(trim(file_name))
      call this%load_url()
      call this%load_size()
      call this%load_response_format()
      call this%load_n()
      call this%load_user_name()
   end subroutine load_ImageEdit_data
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine deallocate_image(this)
      class(ImageEdit), intent(inout) :: this
      if (allocated(this%image)) deallocate(this%image)
   end subroutine deallocate_image
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine deallocate_mask(this)
      class(ImageEdit), intent(inout) :: this
      if (allocated(this%mask)) deallocate(this%mask)
   end subroutine deallocate_mask
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine deallocate_url(this)
      class(ImageEdit), intent(inout) :: this
      if (allocated(this%url)) deallocate(this%url)
   end subroutine deallocate_url
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine deallocate_user_name(this)
      class(ImageEdit), intent(inout) :: this
      if (allocated(this%user_name)) deallocate(this%user_name)
   end subroutine deallocate_user_name
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine deallocate_assistant_response(this)
      class(ImageEdit), intent(inout) :: this
      if (allocated(this%assistant_response)) deallocate(this%assistant_response)
   end subroutine deallocate_assistant_response
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine deallocate_ImageEdit(this)
      class(ImageEdit), intent(inout) :: this
      call this%deallocate_url()
      call this%deallocate_user_name()
      call this%deallocate_assistant_response()
      call this%deallocate_image()
      call this%deallocate_mask()
   end subroutine deallocate_ImageEdit
   !===============================================================================

end module foropenai_ImageEdit
