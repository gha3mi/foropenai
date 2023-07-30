program test3

   use foropenai, only: openai, api_key, set_api_key

   implicit none

   type(openai)        :: oai
   character(len=1000) :: user_input

   api_key = set_api_key('.foropenai')

   call oai%ChatCompletion%create(&
      model='gpt-3.5-turbo',&
      messages=&
      '{}'&
      )

end program test3
