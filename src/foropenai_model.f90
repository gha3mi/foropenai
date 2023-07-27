module foropenai_model

   use foropenai_api_key, only: api_key
   use http,              only: response_type, request, HTTP_GET, pair_type

   implicit none

   private
   public model

   !===============================================================================
   type :: model
      type(response_type) :: response
      character(256)      :: id
   contains
      procedure :: list
      procedure :: retrieve
   end type model
   !===============================================================================

contains

   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine list(this)
      class(model),   intent(inout) :: this
      type(pair_type), allocatable   :: req_header(:)
      character(:),    allocatable   :: json_data

      req_header = [&
         pair_type('Authorization', 'Bearer '//trim(api_key)//'')&
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
   end subroutine list
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   subroutine retrieve(this, id)
      class(model),    intent(inout) :: this
      character(*),    intent(in)    :: id
      type(pair_type), allocatable   :: req_header(:)
      character(:),    allocatable   :: json_data

      req_header = [&
         pair_type('Authorization', 'Bearer '//trim(api_key)//'')&
         ]

      json_data = ''

      this%response = request(&
         url='https://api.openai.com/v1/models/'//trim(id),&
         method=HTTP_GET, data=json_data, header=req_header)

      if (.not. this%response%ok) then
         print *, 'Error message:', this%response%err_msg
         print *, 'Sorry, an error occurred while processing your request.'
      else
         print *, this%response%content
      end if
   end subroutine retrieve
   !===============================================================================

end module foropenai_model

