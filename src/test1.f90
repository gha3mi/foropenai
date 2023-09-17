program test_base

   use foropenai, only: openai

   implicit none

   type(openai) :: base

   call base%set_base_data(file_name='foropenai.json')
   
   call base%print_api_key()
   call base%print_organization()
   call base%print_file_name()

   call base%finalize()

end program test_base
