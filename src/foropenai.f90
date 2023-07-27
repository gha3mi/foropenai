module foropenai

   use foropenai_api_key,        only: api_key, set_api_key
   use foropenai_model,          only: model
   use foropenai_ChatCompletion, only: ChatCompletion
   use http,                     only: response_type, request, HTTP_POST, HTTP_GET, pair_type

   implicit none

   private
   public openai, api_key, set_api_key

   !===============================================================================
   type openai
      character(256)       :: api_key
      type(model)          :: model
      type(ChatCompletion) :: ChatCompletion
   contains
      procedure :: chat_gpt
   end type openai
   !===============================================================================

contains

   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine chat_gpt(this, user_input)
      class(openai),       intent(inout)        :: this
      character(len=1000), intent(in), optional :: user_input
      character(len=1000)                       :: input
      character(:),        allocatable          :: output
      type(pair_type),     allocatable          :: req_header(:)
      character(:),        allocatable          :: json_data

      print *, "Hello! I am ChatGPT."
      print *, "Type exit to end the conversation."
      do
         print *, "You: "
         if (present(user_input)) then
            input = user_input
         else
            read *, input
         end if
         if (input == "exit") exit
         call this%ChatCompletion%create(&
            model=this%model%id,&
            messages = '{' // &
            '"model":"' // trim(this%model%id) // '",' // &
            '"text":"' // trim(input) // '",' // &
            '"messages": [{"role": "user", "content": "Say this is a test!"}]' // &
            '}'&
            )
      end do
      print *, "Goodbye! Have a nice day."
   end subroutine chat_gpt
   !===============================================================================

end module foropenai
