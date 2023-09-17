program test_ChatCompletion

   use foropenai, only: ChatCompletion

   implicit none

   type(ChatCompletion) :: chat

   call chat%set_base_data(file_name='foropenai.json')
   call chat%set(file_name='foropenai.json')

   call chat%init_messages(n=3)
   call chat%messages(1)%set(role='system', content='You are a helpful assistant.')
   call chat%messages(2)%set(role='user', content='Hello?')
   call chat%messages(3)%set(role='assistant', content='')

   call chat%print_user_message()
   call chat%create()
   call chat%print_assistant_response()

   call chat%usage%print()

   call chat%finalize()

end program test_ChatCompletion
