program test_ChatCompletion

   use foropenai, only: ChatCompletion

   implicit none

   type(ChatCompletion) :: chat

   call chat%set_file_name('foropenai.json')
   call chat%print_file_name()
   call chat%load_api_key()
   call chat%print_api_key()
   call chat%load_organization()
   call chat%print_organization()

   call chat%set_url(url='https://api.openai.com/v1/chat/completions')

   call chat%print_model_list()
   call chat%select_model(5)
   call chat%print_model()

   call chat%set_max_tokens(max_tokens=1)
   call chat%print_max_tokens()

   call chat%set_temperature(temperature=0.0)
   call chat%print_temperature()

   call chat%set_user_name(user_name='Ali')
   call chat%print_user_name()

   call chat%init_messages(n=3)
   call chat%messages(1)%set_role(role='system')
   call chat%messages(1)%set_content(content='You are a helpful assistant.')
   call chat%messages(2)%set_role(role='user')
   call chat%messages(2)%set_content(content='Hello?')
   call chat%messages(3)%set_role(role='assistant')
   call chat%messages(3)%set_content(content='')
   call chat%print_user_message()
   call chat%create()
   call chat%print_assistant_response()

   call chat%usage%print_prompt_tokens()
   call chat%usage%print_completion_tokens()
   call chat%usage%print_total_tokens()

   call chat%finalize()

end program test_ChatCompletion
