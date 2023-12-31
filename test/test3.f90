program test_Transcription

   use foropenai, only: Transcription

   implicit none

   type(Transcription) :: trs

   call trs%set_base_data(file_name='foropenai.json')
   call trs%set(file_name='foropenai.json')

   call trs%create(file='test/audio.mp3')
   call trs%print_file()
   call trs%print_assistant_response()

   call trs%finalize()

end program test_Transcription
