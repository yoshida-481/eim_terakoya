package batch.maintenance.domain;

public class SecurityInfoDomain {

	private String SecurityID;
	private String SecurityName;
	private String SecurityEntry;
	private String EntryPriority;
	private String Role_Create;
	private String Role_STATUS_UP;
	private String ROLE_500;
	private String Role_READ;

	public String getSecurityID() {
	    return SecurityID;
	}
	public String getSecurityName() {
	    return SecurityName;
	}
	public String getSecurityEntry() {
	    return SecurityEntry;
	}
	public String getEntryPriority() {
	    return EntryPriority;
	}
	public String getRole_Create() {
	    return Role_Create;
	}
	public String getRole_STATUS_UP() {
	    return Role_STATUS_UP;
	}
	public String getROLE_500() {
	    return ROLE_500;
	}
	public String getRole_READ() {
	    return Role_READ;
	}




	public void setSecurityID(String SecurityID) {
	    this.SecurityID = SecurityID;
	}
	public void setSecurityName(String SecurityName) {
	    this.SecurityName = SecurityName;
	}
	public void setSecurityEntry(String SecurityEntry) {
	    this.SecurityEntry = SecurityEntry;
	}
	public void setEntryPriority(String EntryPriority) {
	    this.EntryPriority = EntryPriority;
	}
	public void setRole_Create(String Role_Create) {
	    this.Role_Create = Role_Create;
	}
	public void setRole_STATUS_UP(String Role_STATUS_UP) {
	    this.Role_STATUS_UP = Role_STATUS_UP;
	}
	public void setROLE_500(String ROLE_500) {
	    this.ROLE_500 = ROLE_500;
	}
	public void setRole_READ(String Role_READ) {
	    this.Role_READ = Role_READ;
	}
}
