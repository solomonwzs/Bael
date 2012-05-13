-module(bael_xml).
-include("bael.hrl").
-export([get_elements/2]).
	
-define(MATH_XML_ELEMENTS_LIST, bael_xml_dict_key_0).
-define(TEST_XML_STRING, "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>
	<!--commit2-->
	<configuration><!--commit0-->
		<application id=\"test\">
			<platform id=\"vk\">
				<appId>123</appId>
				<secretKey>secret</secretKey>
				<packages>
					<package id=\"vk0\" price=\"100\"  points=\"3\" />
					<package id=\"vk1\" price=\"500\"  points=\"20\" />
					<package id=\"vk2\" price=\"1000\" points=\"45\" 
					 default=\"true\" /><!--commit1-->
					<package id=\"vk3\" price=\"2500\" points=\"130\" />
					<package id=\"vk4\" price=\"5000\" points=\"290\" />
				</packages>
			</platform>
			<appId>321</appId>
		</application>
	</configuration>").

%test()->
%	{Element, _}=xmerl_scan:string(?TEST_XML_STRING),
%	io:format("~p~n", [Element]).
%	Ret=get_elements([configuration, package], ?TEST_XML_STRING).
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
	save_elements_to_dict({Tags, Depth, XmlElement}),
	erlang:get(?MATH_XML_ELEMENTS_LIST).

save_elements_to_dict({Tags, Depth, XmlElement})->
	{Type, Tag, _, _, _, Parents, _, Attrs, Content, _, _, _}=XmlElement,
	ParentsList=process_parents(Parents),
	Length=length(ParentsList)+1,
	if
		Type=:=xmlElement andalso (Depth=:=null orelse Depth>=Length)->
			{TextList, ElementList}=lists:foldl(
				fun filter_xml_content/2, 
				{[], []}, 
				Content
			),
			Element=#xml_element{
				tag_name=Tag,
				parents=ParentsList,
				attr=process_attrs(Attrs),
				value=list_to_binary(process_text(TextList))
			},
			[Head|Tail]=lists:reverse(Tags),
			Math=math_tags_list(Tail, ParentsList),
			MathElementsList=erlang:get(?MATH_XML_ELEMENTS_LIST),
			if
				Head=:=Tag andalso Math=:=true->
					erlang:put(
						?MATH_XML_ELEMENTS_LIST, 
					 	lists:append(MathElementsList, [Element])
					);
				true->not_math
			end,
	 		lists:foreach(fun save_elements_to_dict/1,
			 [{Tags, Depth, X}||X<-ElementList]);
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

filter_xml_content(Content, {TextList, ElementList})->
	case hd(tuple_to_list(Content)) of
		xmlText->
			List=lists:append(TextList, [Content]),
			{List, ElementList};
		xmlElement->
			List=lists:append(ElementList, [Content]),
			{TextList, List};
		_->
			{TextList, ElementList}
	end.

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
