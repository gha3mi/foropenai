module foropenai_Transcription

   use foropenai_base

   implicit none

   private
   public :: Transcription

   !===============================================================================
   !> author: Seyed Ali Ghasemi
   type, extends(openai) :: Transcription
      character(len=:), allocatable :: url
      character(len=:), allocatable :: model
      character(len=:), allocatable :: language
      character(len=:), allocatable :: prompt
      character(len=:), allocatable :: file
      character(len=:), allocatable :: text
      character(len=4)              :: response_format='json'
      real                          :: temperature=0.0
   contains
      procedure :: create => create_transcription
      procedure, private :: deallocate_url
      procedure, private :: deallocate_model
      procedure, private :: deallocate_language
      procedure, private :: deallocate_prompt
      procedure, private :: deallocate_file
      procedure, private :: deallocate_text
      procedure :: finalize => deallocate_Transcription
      procedure, private :: load => load_Transcription_data
      procedure, private :: load_url
      procedure, private :: load_model
      procedure, private :: load_temperature
      procedure, private :: load_language
      procedure, private :: load_response_format
      procedure, private :: print_model
      procedure, private :: print_temperature
      procedure, private :: print_language
      procedure :: print_response_format
      procedure, private :: print_prompt
      procedure :: print_assistant_response
      procedure, private :: set_text
      procedure, private :: set_prompt
      procedure, private :: set_url
      procedure, private :: set_model
      procedure, private :: set_temperature
      procedure, private :: set_language
      procedure, private :: set_response_format
      procedure, private :: set_file
      procedure :: set => set_Transcription_data
   end type Transcription
   !===============================================================================

contains

   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine print_assistant_response(this)
      class(Transcription), intent(inout) :: this
      print "('text: ',A)", trim(this%text)
   end subroutine print_assistant_response
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_text(this, text)
      class(Transcription), intent(inout) :: this
      character(len=*),     intent(in)    :: text
      this%text = trim(text)
   end subroutine set_text
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine print_prompt(this)
      class(Transcription), intent(inout) :: this
      print "('prompt: ',A)", trim(this%prompt)
   end subroutine print_prompt
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_prompt(this, prompt)
      class(Transcription), intent(inout) :: this
      character(len=*),     intent(in)    :: prompt
      this%prompt = trim(prompt)
   end subroutine set_prompt
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine deallocate_Transcription(this)
      class(Transcription), intent(inout) :: this
      call this%deallocate_url()
      call this%deallocate_model()
      call this%deallocate_language()
      call this%deallocate_prompt()
      call this%deallocate_file()
      call this%deallocate_text()
   end subroutine deallocate_Transcription
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine deallocate_url(this)
      class(Transcription), intent(inout) :: this
      if (allocated(this%url)) deallocate(this%url)
   end subroutine deallocate_url
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine deallocate_model(this)
      class(Transcription), intent(inout) :: this
      if (allocated(this%model)) deallocate(this%model)
   end subroutine deallocate_model
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine deallocate_language(this)
      class(Transcription), intent(inout) :: this
      if (allocated(this%language)) deallocate(this%language)
   end subroutine deallocate_language
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine deallocate_prompt(this)
      class(Transcription), intent(inout) :: this
      if (allocated(this%prompt)) deallocate(this%prompt)
   end subroutine deallocate_prompt
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine deallocate_file(this)
      class(Transcription), intent(inout) :: this
      if (allocated(this%file)) deallocate(this%file)
   end subroutine deallocate_file
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine deallocate_text(this)
      class(Transcription), intent(inout) :: this
      if (allocated(this%text)) deallocate(this%text)
   end subroutine deallocate_text
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_response_format(this, response_format)
      class(Transcription), intent(inout) :: this
      character(len=*),     intent(in)    :: response_format
      this%response_format = trim(response_format)
   end subroutine set_response_format
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine load_response_format(this)
      use json_module, only: json_file
      class(Transcription), intent(inout) :: this
      type(json_file)                     :: json
      character(len=:), allocatable       :: tmp
      logical                             :: found
      call json%initialize()
      call json%load_file(trim(this%file_name))
      call json%get("Transcription.response_format", tmp, found=found)
      if (found) this%response_format = trim(tmp)
      call json%destroy()
   end subroutine load_response_format
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine print_response_format(this)
      class(Transcription), intent(inout) :: this
      print "('response_format: ',A)", trim(this%response_format)
   end subroutine print_response_format
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine load_language(this)
      use json_module, only: json_file
      class(Transcription), intent(inout) :: this
      type(json_file)                     :: json
      character(len=:), allocatable       :: tmp
      logical                             :: found
      call json%initialize()
      call json%load_file(trim(this%file_name))
      call json%get("Transcription.language", tmp, found=found)
      if (found) this%language = trim(tmp)
      call json%destroy()
   end subroutine load_language
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine print_language(this)
      class(Transcription), intent(inout) :: this
      print "('language: ',A)", trim(this%language)
   end subroutine print_language
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_language(this, language)
      class(Transcription), intent(inout) :: this
      character(len=*),     intent(in)    :: language
      this%language = trim(language)
   end subroutine set_language
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_file(this, file)
      class(Transcription), intent(inout) :: this
      character(len=*),      intent(in)    :: file
      this%file = trim(file)
   end subroutine set_file
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine set_Transcription_data(this, file_name, &
      url, model, temperature, language, response_format)
      class(Transcription),       intent(inout) :: this
      character(len=*), optional, intent(in)    :: file_name
      character(len=*), optional, intent(in)    :: url
      character(len=*), optional, intent(in)    :: model
      real,             optional, intent(in)    :: temperature
      character(len=*), optional, intent(in)    :: language
      character(len=*), optional, intent(in)    :: response_format

      if (present(url)) call this%set_url(url=url)
      if (present(model)) call this%set_model(model=model)
      if (present(temperature)) call this%set_temperature(temperature=temperature)
      if (present(language)) call this%set_language(language=language)
      if (present(response_format)) call this%set_response_format(response_format=response_format)

      if (present(file_name)) then
         call this%set_file_name(file_name)
         call this%load(file_name)
      end if
   end subroutine set_Transcription_data
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine load_Transcription_data(this, file_name)
      class(Transcription), intent(inout) :: this
      character(len=*),     intent(in)    :: file_name
      call this%set_file_name(trim(file_name))
      call this%load_url()
      call this%load_model()
      call this%load_temperature()
      call this%load_language()
      call this%load_response_format()
   end subroutine load_Transcription_data
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine load_temperature(this)
      use json_module, only: json_file
      class(Transcription), intent(inout) :: this
      type(json_file)                     :: json
      real                                :: tmp
      logical                             :: found
      call json%initialize()
      call json%load_file(trim(this%file_name))
      call json%get("Transcription.temperature", tmp, found=found)
      if (found) this%temperature = tmp
      call json%destroy()
   end subroutine load_temperature
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine load_url(this)
      use json_module, only: json_file
      class(Transcription), intent(inout) :: this
      type(json_file)                     :: json
      character(len=:), allocatable       :: tmp
      logical                             :: found
      call json%initialize()
      call json%load_file(trim(this%file_name))
      call json%get("Transcription.url", tmp, found=found)
      if (found) this%url = trim(tmp)
      call json%destroy()
   end subroutine load_url
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine load_model(this)
      use json_module, only: json_file
      class(Transcription), intent(inout) :: this
      type(json_file)                     :: json
      character(len=:), allocatable       :: tmp
      logical                             :: found
      call json%initialize()
      call json%load_file(trim(this%file_name))
      call json%get("Transcription.model", tmp, found=found)
      if (found) this%model = trim(tmp)
      call json%destroy()
   end subroutine load_model
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_temperature(this, temperature)
      class(Transcription), intent(inout) :: this
      real,                 intent(in)    :: temperature
      this%temperature = temperature
   end subroutine set_temperature
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_url(this, url)
      class(Transcription), intent(inout) :: this
      character(len=*),     intent(in)    :: url
      this%url = trim(url)
   end subroutine set_url
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental pure subroutine set_model(this, model)
      class(Transcription), intent(inout) :: this
      character(len=*),     intent(in)    :: model
      this%model = trim(model)
   end subroutine set_model
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine create_transcription(this, file, prompt)
      use http,        only: response_type, request, HTTP_POST, pair_type
      use json_module, only: json_file

      class(Transcription), intent(inout)        :: this
      character(len=*),     intent(in)           :: file
      character(len=*),     intent(in), optional :: prompt
      type(pair_type),      allocatable          :: req_header(:), form_data(:), file_data
      type(response_type)                        :: response
      type(json_file)                            :: json
      character(len=1024)                        :: temperature_str

      call this%set_file(file=file)

      if (present(prompt)) then
         call this%set_prompt(prompt=prompt)
      else
         call this%set_prompt(prompt='')
      end if

      req_header = [&
         pair_type('Authorization', 'Bearer '//trim(this%api_key)//''),&
         pair_type('Content-Type', 'multipart/form-data')&
         ]

      write(temperature_str,'(f3.1)') this%temperature

      form_data = [&
         pair_type('model', trim(this%model)),&
         pair_type(' language', trim(this%language)),&
         pair_type(' response_format', trim(this%response_format)),&
         pair_type(' prompt', trim(this%prompt)),&
         pair_type(' temperature', trim(temperature_str))&
         ]

      file_data =  pair_type('file', trim(this%file))

      response = request(url = this%url, method = HTTP_POST , header = req_header, form=form_data, file=file_data)

      if (response%ok) then
         call json%initialize()
         call json%deserialize(response%content)
         call json%get("text", this%text)
         call json%destroy()
      else
         print '(A)', 'Sorry, an error occurred while processing your request.'
         print '(A)', 'Error message:', response%err_msg
      end if

   end subroutine create_transcription
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine print_model(this)
      class(Transcription), intent(inout) :: this
      print "('model: ',A)", trim(this%model)
   end subroutine print_model
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental impure subroutine print_temperature(this)
      class(Transcription), intent(inout) :: this
      print "('temperature: ',F3.1)", this%temperature
   end subroutine print_temperature
   !===============================================================================

end module foropenai_Transcription
