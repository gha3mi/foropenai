module foropenai_ChatCompletion

   use foropenai_api_key, only: api_key
   use http,              only: response_type, request, HTTP_POST, pair_type

   implicit none

   private
   public ChatCompletion

   !===============================================================================
   type :: ChatCompletion
      type(response_type) :: response
   contains
      procedure :: create
   end type ChatCompletion
   !===============================================================================

contains

   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine create(this, model, messages)
      class(ChatCompletion), intent(inout) :: this
      character(*),          intent(in)    :: model
      character(*),          intent(in)    :: messages
      type(pair_type),       allocatable   :: req_header(:)

      req_header = [&
         pair_type('Content-Type', 'application/json'),&
         pair_type('Authorization', 'Bearer '//api_key//'')&
         ]

      this%response = request(&
         url='https://api.openai.com/v1/chat/completions',&
         method=HTTP_POST, data=messages, header=req_header)

      if (.not. this%response%ok) then
         print *, 'Error message:', this%response%err_msg
         print *, 'Sorry, an error occurred while processing your request.'
      else
         print *, "ChatGPT: "
         print *, this%response%content
      end if

   end subroutine create
   !===============================================================================

end module foropenai_ChatCompletion
