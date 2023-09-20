program test_Translation

   use foropenai, only: Translation

   implicit none

   type(Translation) :: trs

   call trs%set_base_data(file_name='foropenai.json')
   call trs%set(file_name='foropenai.json')

   call trs%create(file='test/audio_de.mp3')
   call trs%print_assistant_response()

   call trs%finalize()

end program test_Translation
