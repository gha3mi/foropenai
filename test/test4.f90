program test_ImageGeneration

   use foropenai, only: ImageGeneration

   implicit none

   type(ImageGeneration) :: image

   call image%set_base_data(file_name='foropenai.json')
   call image%set(file_name='foropenai.json')

   call image%create(prompt='a cat with a computer')
   call image%print_assistant_response()

   call image%finalize()

end program test_ImageGeneration
