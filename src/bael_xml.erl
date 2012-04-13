-module(bael_xml).
-include("bael.hrl").
-export([get_elements/2]).
	
-define(MATH_XML_ELEMENTS_LIST, bael_xml_dict_key_0).
-define(XML_ELEMENT_LIST, bael_xml_dict_key_1).
-define(XML_TEXT_LIST, bael_xml_dict_key_2).
-define(TEST_XML_STRING, "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>
	<configuration>
		<application id=\"test\">
			<platform id=\"vk\">
				<appId>123</appId>
				<secretKey>secret</secretKey>
				<packages>
					<package id=\"vk0\" price=\"100\"  points=\"3\" />
					<package id=\"vk1\" price=\"500\"  points=\"20\" />
					<package id=\"vk2\" price=\"1000\" points=\"45\" 
					 default=\"true\" />
					<package id=\"vk3\" price=\"2500\" points=\"130\" />
					<package id=\"vk4\" price=\"5000\" points=\"290\" />
				</packages>
			</platform>
			<appId>321</appId>
		</application>
	</configuration>").

%test()->
%	Ret=get_elements([configuration, appId], 3, ?TEST_XML_STRING),
%	if
%		Ret=:=[#xml_element{
%			tag_name=appId,
%			parents=[application, configuration],
%			attr=[],
%			value= <<"321">>
%		}]->
%			ok;
%		true->Ret
%	end.

get_elements(Tags, XmlString) when is_list(XmlString)->
	get_elements(Tags, null, XmlString);
get_elements(Tags, XmlElement)->
	get_elements(Tags, null, XmlElement).

get_elements(Tags, Depth, XmlString) when is_list(XmlString)->
	{Element, _}=xmerl_scan:string(XmlString),
	get_elements(Tags, Depth, Element);
get_elements(Tags, Depth, XmlElement)->
	erlang:put(?MATH_XML_ELEMENTS_LIST, []),
	save_elements_to_dict(Tags, Depth, XmlElement),
	erlang:get(?MATH_XML_ELEMENTS_LIST).

save_elements_to_dict(Tags, Depth, XmlElement)->
	{xmlElement, Tag, _, _, _, Parents, _, Attrs, Content, _, _, _}=XmlElement,
	ParentsList=process_parents(Parents),
	Length=length(ParentsList)+1,
	if
		Depth=:=null orelse Depth>=Length->
			erlang:put(?XML_TEXT_LIST, []),
			erlang:put(?XML_ELEMENT_LIST, []),
			filter_xml_content(Content),
			Element=#xml_element{
				tag_name=Tag,
				parents=ParentsList,
				attr=process_attrs(Attrs),
				value=list_to_binary(process_text(erlang:get(?XML_TEXT_LIST)))
			},
			[Head|Tail]=lists:reverse(Tags),
			Math=math_tags_list(Tail, ParentsList),
			MathElementsList=erlang:get(?MATH_XML_ELEMENTS_LIST),
			if
				Head=:=Tag andalso Math=:=true->
					erlang:put(?MATH_XML_ELEMENTS_LIST, 
					 lists:append(MathElementsList, [Element]));
				true->not_math
			end,
			[save_elements_to_dict(Tags, Depth, X)||
			 X<-erlang:get(?XML_ELEMENT_LIST)];
		true->ok
	end.

process_parents([])->
	[];
process_parents(Parents)->
	[Head|Tail]=Parents,
	{Name, _}=Head,
	[Name|process_parents(Tail)].

process_text([])->
	[];
process_text(Texts)->
	[Head|Tail]=Texts,
	{xmlText, _, _, _, Text, _}=Head,
	[Text|process_text(Tail)].

process_attrs([])->
	[];
process_attrs(Attrs)->
	[Head|Tail]=Attrs,
	{xmlAttribute, Key, _, _, _, _, _, _, Value, _}=Head,
	[{Key, Value}|process_attrs(Tail)].

filter_xml_content([])->
	ok;
filter_xml_content(Content)->
	[Head|Tail]=Content,
	Key=case hd(tuple_to_list(Head)) of
		xmlText->?XML_TEXT_LIST;
		xmlElement->?XML_ELEMENT_LIST
	end,
	L=erlang:get(Key),
	erlang:put(Key, lists:append(L, [Head])),
	filter_xml_content(Tail).

math_tags_list([], _Parents)->
	true;
math_tags_list(_Tags, [])->
	false;
math_tags_list(Tags, Parents)->
	[Head1|Tail1]=Tags,
	[Head2|Tail2]=Parents,
	if
		Head1=:=Head2->math_tags_list(Tail1, Tail2);
		true->math_tags_list(Tags, Tail2)
	end.
