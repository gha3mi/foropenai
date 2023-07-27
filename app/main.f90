program prg_forgpt

   use forgpt

   implicit none

   type(openai)        :: gpt
   character(len=1000) :: user_input

   print*,"Create a file called '.forgpt' and place your API key on the first line, and your organization key on the second line."
   print*,"Press Enter to continue..."
   read(*,*)
   call gpt%set_file_name_settings('.forgpt')

   call gpt%set_api_key()
   call gpt%set_org_key()
   
   call gpt%list_models()

   print *, "Hello! I am ChatGPT. type 'exit' to end the conversation."
   do
      print *, "You: "
      read *, user_input
      if (user_input == "exit") exit
      call gpt%create_chat_completion(user_input,'gpt-3.5-turbo','0.7')
   end do
   print *, "Goodbye! Have a nice day."

end program prg_forgpt
