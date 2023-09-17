program main
   
   use foropenai, only: ChatCompletion
   implicit none

   type(ChatCompletion) :: chat

   call chat%conversation(&
      file_name_base           = 'foropenai.json',&
      file_name_ChatCompletion = 'foropenai.json',&
      input_file               = 'chat_input',&
      output_file              = 'chat_history',&
      inputfile_command        = ':ifile',&
      exit_command             = ':q')

   call chat%finalize()

end program main
