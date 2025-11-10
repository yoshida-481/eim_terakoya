package jp.co.ctc_g.eim.app.document.business.domain;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;


/**
 * 
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class DocumentFormDomain {
	private long id;
	private String name;
	private List<DocumentFormAttributeDomain> attributeList = new ArrayList<DocumentFormAttributeDomain>();
	
	public long getId() {
		return id;
	}
	public void setId(long id) {
		this.id = id;
	}
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public List<DocumentFormAttributeDomain> getAttributeList() {
		return attributeList;
	}
	public void setAttributeList(List<DocumentFormAttributeDomain> attributeList) {
		this.attributeList = attributeList;
	}
}
