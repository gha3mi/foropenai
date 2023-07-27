program forgpt

   use foropenai, only: openai, api_key, set_api_key

   implicit none

   type(openai) :: oai

   api_key = set_api_key('.foropenai')
   oai%model%id = 'gpt-3.5-turbo'
   call oai%chat_gpt()

end program forgpt
