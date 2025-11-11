package eim.command.business.service.response.impl;

import java.text.SimpleDateFormat;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import eim.bo.EIMObject;
import eim.command.business.service.result.EIMCommandResult;
import eim.util.EIMConfig;

public class EIMCommandDefaultResponseMaker extends EIMCommandResponseMakerDOM {

	@Override
	protected Document makeResponseContentsByDOM(EIMCommandResult resultData)
			throws Exception {
		//DocumentBuilder
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		DocumentBuilder db = dbf.newDocumentBuilder();
		Document doc = db.newDocument();
		
		//Root Element
		Element root = doc.createElement("result");
		doc.appendChild(root);
		
		//Type
		Element type = doc.createElement("type");
		type.appendChild(doc.createTextNode(resultData.getType()));
		root.appendChild(type);
		
		//Code
		Element code = doc.createElement("code");
		code.appendChild(doc.createTextNode(resultData.getCode()));
		root.appendChild(code);
		
		//Message
		Element message = doc.createElement("message");
		message.appendChild(doc.createTextNode(resultData.getMessage()));
		root.appendChild(message);
		
		//Data
		EIMObject[] data = resultData.getData();
		if(data != null) {
			
			//Date Format
			SimpleDateFormat sdf = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
			
			//Data Element
			Element dataElement = doc.createElement("data");
			
			for(int i = 0; i < data.length; i++) {
				
				//Object Element
				Element object = doc.createElement("object");
				
				//ID
				Element objId = doc.createElement("id");
				objId.appendChild(doc.createTextNode(String.valueOf(data[i].getId())));
				object.appendChild(objId);
				
				//Type
				Element objTypeName = doc.createElement("class");
				objTypeName.appendChild(doc.createTextNode(data[i].getType().getName()));
				object.appendChild(objTypeName);
				
				//Name
				Element objName = doc.createElement("name");
				objName.appendChild(doc.createTextNode(data[i].getName()));
				object.appendChild(objName);
				
				//Owner
				Element owner = doc.createElement("owner");
				owner.appendChild(doc.createTextNode(data[i].getModifyUser().getName()));
				object.appendChild(owner);
				
				//Status
				Element status = doc.createElement("status");
				if(data[i].getStatus() != null) {
					status.appendChild(doc.createTextNode(data[i].getStatus().getType().getName()));
				}
				object.appendChild(status);
				
				//Update
				Element update = doc.createElement("update");
				update.appendChild(doc.createTextNode(sdf.format(data[i].getModifyDate())));
				object.appendChild(update);
				
				//Signed
				Element signed = doc.createElement("signed");
				if(data[i].getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS")) != null) {
					signed.appendChild(doc.createTextNode(String.valueOf(data[i].getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS")).getInt())));
				}
				object.appendChild(signed);
				
				//Close Object Element
				dataElement.appendChild(object);
				
			}
			
			//Close Data Element
			root.appendChild(dataElement);
			
		}
		return doc;
	}

}
