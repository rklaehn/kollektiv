A sequence diagram. Note that you need to include ; in each new line:

![Alt text](http://g.gravizo.com/g?
@startuml;
actor User;
participant "ChatThreadActor receive" as A;
participant "thread pool" as B;
User -> A: NewChatMessage;
activate A;
A -> B: prepareGroupChatMessage;
activate B;
B --> A: GroupChatMessageProcessed;
deactivate B;
A -> B: respondToGroupChatMessage;
activate B;
B --> A: SgResult[HalChatMessage];
deactivate B;
A --> User: SgResult[HalChatMessage];
deactivate A;
@enduml
)
