program test1

   use foropenai, only: openai, api_key, set_api_key

   implicit none

   type(openai)        :: oai
   character(len=1000) :: user_input

   api_key = set_api_key('.foropenai')

   call oai%model%list()

end program test1
