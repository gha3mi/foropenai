module forgpt

   use http, only: response_type, request, HTTP_POST, HTTP_GET, pair_type

   implicit none

   private
   public openai

   !===============================================================================
   type openai
      character(256)      :: api_key
      character(256)      :: org_key
      character(256)      :: file_name_settings
      type(response_type) :: response
      character(256)      :: model
      character(256)      :: temperature
   contains
      procedure :: set_model
      procedure :: set_temperature
      procedure :: set_file_name_settings
      procedure :: set_api_key
      procedure :: set_org_key
      procedure :: list_models
      procedure :: create_chat_completion
   end type openai
   !===============================================================================

contains

   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine set_temperature(this,temperature)
      class(openai), intent(inout) :: this
      character(*),  intent(in)    :: temperature

      this%temperature = temperature
   end subroutine set_temperature
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine set_model(this,model)
      class(openai), intent(inout) :: this
      character(*),  intent(in)    :: model

      this%model = model
   end subroutine set_model
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine set_file_name_settings(this,filename)
      class(openai), intent(inout) :: this
      character(*),  intent(in)    :: filename

      this%file_name_settings = filename
   end subroutine set_file_name_settings
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine set_api_key(this,api_key)
      class(openai), intent(inout)        :: this
      character(*),  intent(in), optional :: api_key
      integer                             :: nunit, n

      if (present(api_key)) then
         this%api_key = api_key
      else
         open(newunit=nunit, file=this%file_name_settings, status="old", action="read", iostat=n)
         if (n /= 0) then
            print*, "error opening the file: ", trim(this%file_name_settings)
            stop
         end if
         read(nunit, '(a)', iostat=n) this%api_key
         close(nunit)
      end if
   end subroutine set_api_key
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine set_org_key(this,org_key)
      class(openai), intent(inout)        :: this
      character(*),  intent(in), optional :: org_key
      integer                             :: nunit, n

      if (present(org_key)) then
         this%org_key = org_key
      else
         open(newunit=nunit, file=this%file_name_settings, status="old", action="read", iostat=n)
         if (n /= 0) then
            print*, "error opening the file: ", trim(this%file_name_settings)
            stop
         end if
         read(nunit, '(a)', iostat=n)
         read(nunit, '(a)', iostat=n) this%org_key
         close(nunit)
      end if
   end subroutine set_org_key
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine list_models(this)
      class(openai),   intent(inout) :: this
      type(pair_type), allocatable   :: req_header(:)
      character(:),    allocatable   :: json_data

      req_header = [&
         pair_type('Authorization', 'Bearer '//trim(this%api_key)//''),&
         pair_type('OpenAI-Organization ', ''//trim(this%org_key)//'')&
         ]

      json_data = ''

      this%response = request(&
         url='https://api.openai.com/v1/models',&
         method=HTTP_GET, data=json_data, header=req_header)

      if (.not. this%response%ok) then
         print *, 'Error message:', this%response%err_msg
         print *, 'Sorry, an error occurred while processing your request.'
      else
         print *, this%response%content
      end if
   end subroutine list_models
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine create_chat_completion(this, input_text, model, temperature)
      class(openai),   intent(inout) :: this
      character(*),    intent(in)    :: input_text
      character(*),    intent(in)    :: model
      character(*),    intent(in)    :: temperature
      character(:),    allocatable   :: output
      type(pair_type), allocatable   :: req_header(:)
      character(:),    allocatable   :: json_data

      call this%set_model(model)
      call this%set_temperature(temperature)

      req_header = [&
         pair_type('Content-Type', 'application/json'),&
         pair_type('Authorization', 'Bearer '//this%api_key//''),&
         pair_type('OpenAI-Organization ', ''//this%org_key//'')&
         ]

      json_data = '{' // &
         '"model":"' // trim(this%model) // '",' // &
         '"text":"' // trim(input_text) // '",' // &
         '"messages": [{"role": "user", "content": "Say this is a test!"}], ' // &
         '"temperature":"' // trim(this%temperature) // '"' // &
         '}'

      this%response = request(&
         url='https://api.openai.com/v1/chat/completions',&
         method=HTTP_POST, data=json_data, header=req_header)

      if (.not. this%response%ok) then
         print *, 'Error message:', this%response%err_msg
         print *, 'Sorry, an error occurred while processing your request.'
      else
         print *, "ChatGPT: "
         print *, this%response%content
      end if
   end subroutine create_chat_completion
   !===============================================================================

end module forgpt
