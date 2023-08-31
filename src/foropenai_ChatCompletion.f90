module foropenai_ChatCompletion

   use foropenai_base

   implicit none

   private
   public :: ChatCompletion

   !===============================================================================
   !> author: Seyed Ali Ghasemi
   type usage
      integer :: prompt_tokens=0
      integer :: completion_tokens=0
      integer :: total_tokens=0
   contains
      procedure :: print_prompt_tokens
      procedure :: print_completion_tokens
      procedure :: print_total_tokens
   end type usage
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   type :: ChatCompletion_messages
      character(len=:), allocatable :: role
      character(len=:), allocatable :: content
      character(len=:), allocatable :: name
   contains
      procedure :: deallocate_role
      procedure :: deallocate_content
      procedure :: deallocate_name
      procedure :: finalize => deallocate_ChatCompletion_messages
      procedure :: set => set_message
      procedure :: set_role
      procedure :: set_content
      procedure :: set_name
   end type ChatCompletion_messages
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   type, extends(openai) :: ChatCompletion
      character(len=:),              allocatable :: url
      character(len=:),              allocatable :: model
      character(len=:),              allocatable :: user_name
      character(len=256),            allocatable :: model_list(:)
      type(ChatCompletion_messages), allocatable :: messages(:)
      integer                                    :: max_tokens
      type(usage)                                :: usage
      real                                       :: temperature
      character(len=:),              allocatable :: finish_reason
   contains
      procedure :: check => check_chat_completion
      procedure :: create => create_chat_completion
      procedure :: conversation
      procedure :: deallocate_messages
      procedure :: deallocate_model_list
      procedure :: deallocate_url
      procedure :: deallocate_model
      procedure :: deallocate_user_name
      procedure :: deallocate_finish_reason
      procedure :: finalize => deallocate_ChatCompletion
      procedure :: init_messages
      procedure :: load_ChatCompletion_data
      procedure :: load_user_name
      procedure :: load_url
      procedure :: load_model
      procedure :: load_temperature
      procedure :: load_max_tokens
      procedure :: read_msg
      procedure :: print_user_name
      procedure :: print_model_list
      procedure :: print_model
      procedure :: print_temperature
      procedure :: print_max_tokens
      procedure :: set_user_name
      procedure :: set_url
      procedure :: set_model
      procedure :: set_model_list
      procedure :: select_model
      procedure :: set_temperature
      procedure :: set_max_tokens
      procedure :: write_history
      procedure :: print_finish_reason
   end type ChatCompletion
   !===============================================================================

contains

   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental subroutine deallocate_finish_reason(this)
      class(ChatCompletion), intent(inout) :: this
      if (allocated(this%finish_reason)) deallocate(this%finish_reason)
   end subroutine deallocate_finish_reason
   !===============================================================================
   

   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine print_finish_reason(this)
      class(ChatCompletion), intent(inout) :: this
      print "('finish reason: ',A)", trim(this%finish_reason)
   end subroutine print_finish_reason
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine conversation(this,config_file, input_file, output_file, inputfile_command, exit_command)
      use face, only: colorize

      class(ChatCompletion), intent(inout) :: this
      character(len=:), allocatable        :: msg
      character(len=:), allocatable        :: rsp
      character(len=:), allocatable        :: whole_msg
      integer                              :: iounit
      character(len=*), intent(in)         :: config_file
      character(len=*), intent(in)         :: input_file
      character(len=*), intent(in)         :: output_file
      character(len=*), intent(in)         :: inputfile_command
      character(len=*), intent(in)         :: exit_command

      call this%load_base_data(config_file)
      call this%load_ChatCompletion_data(config_file)

      call this%init_messages(n=3)
      call this%messages(1)%set(role='system', content='You are a helpful assistant.')
      call this%messages(2)%set(role='assistant', content='Hello!')
      call this%messages(3)%set_role(role='user')

      do
         call this%read_msg(message=msg, file_name=trim(input_file), command=trim(inputfile_command))
         if (trim(msg) == trim(exit_command)) exit
         call this%create(msg=trim(msg), rsp=rsp, conversiation=.true.)
         call this%write_history(file_name=trim(output_file), message=trim(msg), response=rsp)
      end do

      call this%usage%print_prompt_tokens()
      call this%usage%print_completion_tokens()
      call this%usage%print_total_tokens()
   end subroutine conversation
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine write_history(this, message, response, file_name)
      class(ChatCompletion), intent(inout) :: this
      character(len=*),      intent(in)    :: response
      character(len=*),      intent(in)    :: message
      character(len=*),      intent(in)    :: file_name
      integer                              :: iounit

      open(unit=iounit, file=trim(file_name), status='unknown', access='append', action='write')
      write(iounit,"('User: ',A)") message
      write(iounit,"('ChatGPT: ',A)") response
      close(iounit)
   end subroutine write_history
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine read_msg(this, message, file_name, command)
      use face, only: colorize
      class(ChatCompletion),         intent(inout) :: this
      character(len=:), allocatable, intent(out)   :: message
      character(len=*),              intent(in)    :: file_name
      character(len=*),              intent(in)    :: command
      character(len=1000000)                       :: tmp
      integer                                      :: iounit, iostat

      write(*,"(A,': ')",advance='no') colorize(trim(this%user_name), color_bg='green')
      read*, tmp
      message = trim(tmp)
      if (trim(message) == trim(command)) then
         open(unit=iounit, file=trim(file_name), status='old', action='read')
         do
            read(iounit,'(A)',iostat=iostat) tmp
            if (iostat /= 0) exit
            message = trim(message) // trim(tmp)  // new_line(' ')
         end do
         close(iounit)
      end if
   end subroutine read_msg
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine deallocate_ChatCompletion_messages(this)
      class(ChatCompletion_messages), intent(inout) :: this
      call this%deallocate_role()
      call this%deallocate_content()
      call this%deallocate_name()
   end subroutine deallocate_ChatCompletion_messages
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine deallocate_ChatCompletion(this)
      class(ChatCompletion), intent(inout) :: this
      call this%deallocate_messages()
      call this%deallocate_model_list()
      call this%deallocate_url()
      call this%deallocate_model()
      call this%deallocate_user_name()
      call this%deallocate_finish_reason()
   end subroutine deallocate_ChatCompletion
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental subroutine deallocate_url(this)
      class(ChatCompletion), intent(inout) :: this
      if (allocated(this%url)) deallocate(this%url)
   end subroutine deallocate_url
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental subroutine deallocate_model(this)
      class(ChatCompletion), intent(inout) :: this
      if (allocated(this%model)) deallocate(this%model)
   end subroutine deallocate_model
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental subroutine deallocate_user_name(this)
      class(ChatCompletion), intent(inout) :: this
      if (allocated(this%user_name)) deallocate(this%user_name)
   end subroutine deallocate_user_name
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine load_ChatCompletion_data(this, file_name)
      class(ChatCompletion), intent(inout) :: this
      character(len=*),      intent(in)    :: file_name
      call this%set_file_name(trim(file_name))
      call this%load_url()
      call this%load_model()
      call this%load_user_name()
      call this%load_temperature()
      call this%load_max_tokens()
   end subroutine load_ChatCompletion_data
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine set_message(this, role, content, name)
      class(ChatCompletion_messages), intent(inout) :: this
      character(len=*),               intent(in)    :: role
      character(len=*),               intent(in)    :: content
      character(len=*), optional,     intent(in)    :: name
      this%role = trim(role)
      this%content = trim(content)
      if (present(name)) this%name = trim(name)
   end subroutine set_message
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine load_max_tokens(this)
      use json_module, only: json_file
      class(ChatCompletion), intent(inout) :: this
      type(json_file)                      :: json
      call json%load_file(trim(this%file_name))
      call json%get("ChatCompletion.max_tokens", this%max_tokens)
   end subroutine load_max_tokens
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine load_temperature(this)
      use json_module, only: json_file
      class(ChatCompletion), intent(inout) :: this
      type(json_file)                      :: json
      call json%load_file(trim(this%file_name))
      call json%get("ChatCompletion.temperature", this%temperature)
   end subroutine load_temperature
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine load_url(this)
      use json_module, only: json_file
      class(ChatCompletion), intent(inout) :: this
      type(json_file)                      :: json
      call json%load_file(trim(this%file_name))
      call json%get("ChatCompletion.url", this%url)
   end subroutine load_url
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine load_model(this)
      use json_module, only: json_file
      class(ChatCompletion), intent(inout) :: this
      type(json_file)                      :: json
      call json%load_file(trim(this%file_name))
      call json%get("ChatCompletion.model", this%model)
   end subroutine load_model
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine load_user_name(this)
      use json_module, only: json_file
      class(ChatCompletion), intent(inout) :: this
      type(json_file)                      :: json
      call json%load_file(trim(this%file_name))
      call json%get("ChatCompletion.user_name", this%user_name)
   end subroutine load_user_name
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental subroutine deallocate_model_list(this)
      class(ChatCompletion), intent(inout) :: this
      if (allocated(this%model_list)) deallocate(this%model_list)
   end subroutine deallocate_model_list
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine set_model_list(this)
      class(ChatCompletion), intent(inout) :: this
      if (.not. allocated(this%model_list)) allocate(this%model_list(8))
      this%model_list(1) = trim('gpt-4')
      this%model_list(2) = trim('gpt-4-0613')
      this%model_list(3) = trim('gpt-4-32k')
      this%model_list(4) = trim('gpt-4-32k-0613')
      this%model_list(5) = trim('gpt-3.5-turbo')
      this%model_list(6) = trim('gpt-3.5-turbo-0613')
      this%model_list(7) = trim('gpt-3.5-turbo-16k')
      this%model_list(8) = trim('gpt-3.5-turbo-16k-0613')
   end subroutine set_model_list
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine print_model_list(this)
      class(ChatCompletion), intent(inout) :: this
      integer                              :: i
      call this%set_model_list()
      do i = 1, size(this%model_list)
         print "(I1,': ',A)",i,trim(this%model_list(i))
      end do
   end subroutine print_model_list
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental subroutine set_temperature(this, temperature)
      class(ChatCompletion), intent(inout) :: this
      real,                  intent(in)    :: temperature
      this%temperature = temperature
   end subroutine set_temperature
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental subroutine set_url(this, url)
      class(ChatCompletion), intent(inout) :: this
      character(len=*),      intent(in)    :: url
      this%url = trim(url)
   end subroutine set_url
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental subroutine set_model(this, model)
      class(ChatCompletion), intent(inout) :: this
      character(len=*),      intent(in)    :: model
      this%model = trim(model)
   end subroutine set_model
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental subroutine init_messages(this, n)
      class(ChatCompletion), intent(inout) :: this
      integer,               intent(in)    :: n
      if (.not. allocated(this%messages)) allocate(this%messages(n))
   end subroutine init_messages
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental subroutine set_role(this, role)
      class(ChatCompletion_messages), intent(inout) :: this
      character(len=*),               intent(in)    :: role
      this%role = role
   end subroutine set_role
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental subroutine set_content(this, content)
      class(ChatCompletion_messages), intent(inout) :: this
      character(len=*),               intent(in)    :: content
      this%content = content
   end subroutine set_content
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental subroutine set_name(this, name)
      class(ChatCompletion_messages), intent(inout) :: this
      character(len=*),               intent(in)    :: name
      this%name = name
   end subroutine set_name
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine check_chat_completion(this, error)
      class(ChatCompletion), intent(inout) :: this
      integer,               intent(out)   :: error

      if (len_trim(this%api_key) == 0) then
         print '(A)', 'Error: api_key is not set.'
         error = 1
         stop
      end if

      if (len_trim(this%url) == 0) then
         print '(A)', 'Error: url is not set.'
         error = 2
         stop
      end if

      if (len_trim(this%model) == 0) then
         print '(A)', 'Error: model is not set.'
         error = 3
         stop
      end if

      if (.not. allocated(this%messages)) then
         print '(A)', 'Error: messages is not set.'
         error = 4
         stop
      end if

      if (size(this%messages) < 2) then
         print '(A)', 'Error: messages must have at least 2 elements.'
         error = 5
         stop
      end if

      if (.not. allocated(this%messages(1)%role)) then
         print '(A)', 'Error: messages(1)%role is not set.'
         error = 6
         stop
      end if

      if (.not. allocated(this%messages(1)%content)) then
         print '(A)', 'Error: messages(1)%content is not set.'
         error = 7
         stop
      end if

      if (.not. allocated(this%messages(2)%role)) then
         print '(A)', 'Error: messages(2)%role is not set.'
         error = 8
         stop
      end if

      if (.not. allocated(this%messages(2)%content)) then
         print '(A)', 'Error: messages(2)%content is not set.'
         error = 9
         stop
      end if

      if (this%temperature < 0.0 .or. this%temperature > 2.0) then
         print '(A)', 'Error: temperature must be between 0.0 and 2.0.'
         error = 10
         stop
      end if

      if (this%max_tokens < 1) then
         print '(A)', 'Error: max_tokens must be greater than 1'
         error = 11
         stop
      end if

      if (len_trim(this%user_name) == 0) then
         print '(A)', 'Error: user_name is not set.'
         error = 12
         stop
      end if

      error = 0

   end subroutine check_chat_completion
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine create_chat_completion(this,msg,rsp,conversiation)
      use http,        only: response_type, request, HTTP_POST, pair_type
      use json_module, only: json_file
      use face,        only: colorize

      class(ChatCompletion),         intent(inout) :: this
      character(len=*),              intent(in)    :: msg
      logical,                       intent(in)    :: conversiation
      character(len=:), allocatable, intent(out)   :: rsp
      character(len=:), allocatable                :: jsonstr
      type(pair_type),  allocatable                :: req_header(:)
      type(response_type)                          :: response
      type(json_file)                              :: json
      logical                                      :: found
      integer                                      :: i
      character(len=10)                            :: i_str
      integer                                      :: error

      call this%check(error)
      if (error == 0) then

         if (conversiation) then
            do i = 1, size(this%messages)
               if (this%messages(i)%role == 'user') then
                  call this%messages(i)%set_content(trim(msg))
               end if
            end do
            call this%messages(3)%set(role='user', content=trim(msg))

         else
            do i = 1, size(this%messages)
               if (this%messages(i)%role == 'user') then
                  print "(A,': ',A)", colorize(trim(this%user_name), color_bg='green'), this%messages(2)%content
               end if
            end do
         end if

         req_header = [&
            pair_type('Content-Type', 'application/json'),&
            pair_type('Authorization', 'Bearer '//trim(this%api_key)//''),&
            pair_type('OpenAI-Organization', ' '//trim(this%organization)//'')&
            ]

         call json%add('model', trim(this%model))
         do i = 1, size(this%messages)
            write (i_str, "(I10)") i
            call json%add('messages('//trim(i_str)//').role', this%messages(i)%role)
            call json%add('messages('//trim(i_str)//').content', this%messages(i)%content)
            ! call json%add('messages('//trim(i_str)//').name', this%messages(i)%name)
         end do
         call json%add('user', trim(this%user_name))
         call json%add('temperature', this%temperature)
         call json%add('max_tokens', this%max_tokens)
         call json%print_to_string(jsonstr)

         response = request(&
            url = trim(this%url),&
            method = HTTP_POST,&
            data = jsonstr,&
            header = req_header)

         if (response%ok) then
            json = response%content

            call json%get("choices(1).finish_reason", this%finish_reason)
            
            call json%get("usage.prompt_tokens", this%usage%prompt_tokens)
            call json%get("usage.completion_tokens", this%usage%completion_tokens)
            call json%get("usage.total_tokens", this%usage%total_tokens)

            call json%get("choices(1).message.content", rsp, found=found)
            if (found) then
               print "(A,': ',A)", colorize("ChatGPT", color_bg='blue'), rsp
            else
               call json%get("error.message", jsonstr)
               print "(A,': ',A)", colorize("ChatGPT", color_bg='red'), jsonstr
            end if
         else
            print '(A)', 'Sorry, an error occurred while processing your request.'
            print '(A)', 'Error message:', response%err_msg
         end if
      end if

      if (conversiation) then
         do i = 1, size(this%messages)
            if (this%messages(i)%role == 'assistant') then
               call this%messages(i)%set_content(content=rsp)
            end if
         end do
      end if

   end subroutine create_chat_completion
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental subroutine deallocate_role(this)
      class(ChatCompletion_messages), intent(inout) :: this
      if (allocated(this%role)) deallocate(this%role)
   end subroutine deallocate_role
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental subroutine deallocate_content(this)
      class(ChatCompletion_messages), intent(inout) :: this
      if (allocated(this%content)) deallocate(this%content)
   end subroutine deallocate_content
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental subroutine deallocate_name(this)
      class(ChatCompletion_messages), intent(inout) :: this
      if (allocated(this%name)) deallocate(this%name)
   end subroutine deallocate_name
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental subroutine deallocate_messages(this)
      class(ChatCompletion), intent(inout) :: this
      if (allocated(this%messages)) deallocate(this%messages)
   end subroutine deallocate_messages
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine select_model(this,n)
      class(ChatCompletion), intent(inout) :: this
      integer,               intent(in)    :: n
      call this%set_model_list()
      this%model = this%model_list(n)
   end subroutine select_model
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine print_model(this)
      class(ChatCompletion), intent(inout) :: this
      print "('model: ',A)", trim(this%model)
   end subroutine print_model
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine print_temperature(this)
      class(ChatCompletion), intent(inout) :: this
      print "('temperature: ',F3.1)", this%temperature
   end subroutine print_temperature
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental subroutine set_max_tokens(this, max_tokens)
      class(ChatCompletion), intent(inout) :: this
      integer,               intent(in)    :: max_tokens
      this%max_tokens = max_tokens
   end subroutine set_max_tokens
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine print_max_tokens(this)
      class(ChatCompletion), intent(inout) :: this
      print "('max tokens: ',I4)", this%max_tokens
   end subroutine print_max_tokens
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   elemental subroutine set_user_name(this, user_name)
      class(ChatCompletion), intent(inout) :: this
      character(len=*),      intent(in)    :: user_name
      this%user_name = trim(user_name)
   end subroutine set_user_name
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine print_user_name(this)
      class(ChatCompletion), intent(inout) :: this
      print "('user name: ',A)", trim(this%user_name)
   end subroutine print_user_name
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine print_prompt_tokens(this)
      class(usage), intent(inout) :: this
      print "('prompt tokens: ',g0)", this%prompt_tokens
   end subroutine print_prompt_tokens
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine print_completion_tokens(this)
      class(usage), intent(inout) :: this
      print "('completion tokens: ',g0)", this%completion_tokens
   end subroutine print_completion_tokens
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine print_total_tokens(this)
      class(usage), intent(inout) :: this
      print "('total tokens: ',g0)", this%total_tokens
   end subroutine print_total_tokens
   !===============================================================================

end module foropenai_ChatCompletion
