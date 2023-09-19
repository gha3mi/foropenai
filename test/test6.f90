program test_ImageEdit

   use foropenai, only: ImageEdit

   implicit none

   type(ImageEdit) :: image

   call image%set_base_data(file_name='foropenai.json')
   call image%set(file_name='foropenai.json')

   call image%create(image='test/image.png', mask='', prompt='a brown cat with a computer')
   call image%print_assistant_response()

   call image%finalize()

end program test_ImageEdit
