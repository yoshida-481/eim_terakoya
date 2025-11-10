<%@ page language="java" contentType="text/xml; charset=UTF-8" pageEncoding="UTF-8"
%><%@ include file="/WEB-INF/jsp/includes/jsp_header.jsp"

%><c:set var="resultData" value="${resultData}" 

/><?xml version="1.0" encoding="UTF-8"?>
<result>
	<type>
		<c:out value="${resultData.type}" escapeXml="true"/>
	</type>
	<code>
		<c:out value="${resultData.code}" escapeXml="true"/>
	</code>
	<message>
		<c:out value="${resultData.message}" escapeXml="true"/>
	</message>
	<data
		offset="<c:out value="${resultData.offset}" escapeXml="true"/>"
	>
		<object>
			<id>
		 		<c:out value="${resultData.target.id}" escapeXml="true"/>
			</id>
			<class>
				<c:out value="${resultData.target.type.name}" escapeXml="true"/>
			</class>
			<name>
				<c:out value="${resultData.target.name}" escapeXml="true"/>
			</name>
			<creator>
				<c:out value="${resultData.target.createUser.name}" escapeXml="true"/>
			</creator>
			
			<create
				posixTime="<c:out value="${resultData.target.createDate.time}" escapeXml="true"/>"  
			>
				<fmt:formatDate value="${resultData.target.createDate}" pattern="yyyy/MM/dd HH:mm:ss"/>
			</create>
			<owner>
				<c:out value="${resultData.target.modifyUser.name}" escapeXml="true"/>
			</owner>
			<c:if test="${resultData.kind=='d' || resultData.kind=='f'}">
				<status>
					<c:out value="${resultData.target.status.type.name}" escapeXml="true"/>
				</status>
			</c:if>
			<update
				posixTime="<c:out value="${resultData.target.modifyDate.time}" escapeXml="true"/>"  
			>
				<fmt:formatDate value="${resultData.target.modifyDate}" pattern="yyyy/MM/dd HH:mm:ss"/>
			</update>
			<signed>
				<c:out value="${resultData.signed}" escapeXml="true"/>
			</signed>
			<kind>
				<c:out value="${resultData.kind}" escapeXml="true"/>
			</kind>
			<path>
				<c:out value="${resultData.path}" escapeXml="true"/>
			</path>
			<c:if test="${resultData.kind=='d'}">
				<rev>
					<c:out value="${resultData.target.rev}" escapeXml="true"/>
				</rev>
				<fsize>
					<c:out value="${resultData.fsize}" escapeXml="true"/>
				</fsize>
			</c:if>
			<security>
				<c:out value="${resultData.target.security.name}" escapeXml="true"/>
			</security>
			<attList>
				<c:forEach items="${resultData.target.attributeList}" var="att">
				<attribute>
					<id>
				 		<c:out value="${att.type.id}" escapeXml="true"/>
					</id>
					<name>
						<c:out value="${att.type.name}" escapeXml="true"/>
					</name>
					<valueList>
						<c:choose>
							<c:when test="${att.type.valueType.id==1}">
								<c:if test="${att.type.multiple==false}">
									<value>
										<c:out value="${att['int']}" escapeXml="true"/>
									</value>
								</c:if>
								<c:if test="${att.type.multiple==true}">
									<c:forEach items="${att.ints}" var="attValue">
									<value>
										<c:out value="${attValue}" escapeXml="true"/>
									</value>
									</c:forEach>
								</c:if>
							</c:when>
							<c:when test="${att.type.valueType.id==2}">
								<c:if test="${att.type.multiple==false}">
									<value>
										<c:out value="${att.string}" escapeXml="true"/>
									</value>
								</c:if>
								<c:if test="${att.type.multiple==true}">
									<c:forEach items="${att.strings}" var="attValue">
									<value>
										<c:out value="${attValue}" escapeXml="true"/>
									</value>
									</c:forEach>
								</c:if>
							</c:when>
							<c:when test="${att.type.valueType.id==3}">
								<c:if test="${att.type.multiple==false}">
									<value
										posixTime="<c:out value="${att.date.time}" escapeXml="true"/>"  
									>
										<fmt:formatDate value="${att.date}" pattern="yyyy/MM/dd HH:mm:ss"/>
									</value>
								</c:if>
								<c:if test="${att.type.multiple==true}">
									<c:forEach items="${att.dates}" var="attValue">
									<value
										posixTime="<c:out value="${attValue.time}" escapeXml="true"/>"  
									>
										<fmt:formatDate value="${attValue}" pattern="yyyy/MM/dd HH:mm:ss"/>
									</value>
									</c:forEach>
								</c:if>
							</c:when>
							<c:when test="${att.type.valueType.id==4}">
								<c:if test="${att.type.multiple==false}">
									<value>
										<c:out value="${att.text}" escapeXml="true"/>
									</value>
								</c:if>
								<c:if test="${att.type.multiple==true}">
									<c:forEach items="${att.texts}" var="attValue">
									<value>
										<c:out value="${attValue}" escapeXml="true"/>
									</value>
									</c:forEach>
								</c:if>
							</c:when>
							<c:when test="${att.type.valueType.id==5}">
								<c:if test="${att.type.multiple==false}">
									<value>
										<c:out value="${att['double']}" escapeXml="true"/>
									</value>
								</c:if>
								<c:if test="${att.type.multiple==true}">
									<c:forEach items="${att.doubles}" var="attValue">
									<value>
										<c:out value="${attValue}" escapeXml="true"/>
									</value>
									</c:forEach>
								</c:if>
							</c:when>
						</c:choose>
					</valueList>
				</attribute>
				</c:forEach>
			</attList>
		</object>
	</data>
</result>