package eim.command.business.service.execute;

import eim.bo.EIMObject;
import eim.command.business.service.result.EIMCommandResult;
import eim.command.common.util.DocumentManagementUtils;
import eim.command.common.util.EIMCommandResultConstant;
import eim.net.EIMSession;

public class EIMCommandRmExecuter extends EIMCommandExecuter {

	@Override
	public EIMCommandResult execute() throws Exception {
		//Session
		EIMSession sess = super.getSess();
		if(sess == null) {
			throw new Exception(EIMCommandResultConstant.MSG_AUTH_FAILED);
		}
		
		EIMCommandResult result = new EIMCommandResult();
		
		//Get Path
		String path = super.getPath();
		if(path == null || path.equals("")) {
			throw new Exception(EIMCommandResultConstant.MSG_SYSTEM_ERROR);
		}
		
		//When Path Ends with Slash, then Trim It
		if(path.endsWith("/")) {
			path = path.substring(0, path.length() - 1);
		}
		
		//Directory Name and File Name
		String directory = "/";
		String name = path;
		if(path.lastIndexOf("/") != -1) {
			directory = path.substring(0, path.lastIndexOf("/") + 1);
			name = path.substring(path.lastIndexOf("/") + 1);
		}
		
		//Get Option
		String option = super.getOption();
		
		//DocumentManagementService
		DocumentManagementUtils dmu = new DocumentManagementUtils(sess);
		
		//Get Object
		EIMObject object = dmu.getObjectByPathAndName(directory, name);
		if(object == null) {
			result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR, EIMCommandResultConstant.CODE_OBJECT_NOT_FOUND, EIMCommandResultConstant.MSG_OBJECT_NOT_FOUND + "(" + path + ")");
			return result;
		}
		
		//Check Option And Object Type
		if(dmu.isFolderTypes(object.getType()) && (option == null || !option.equalsIgnoreCase("R"))) {
			result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR, EIMCommandResultConstant.CODE_SYSTEM_ERROR, path + " " + EIMCommandResultConstant.MSG_OBJECT_TYPE_DIRECTORY);
			return result;
		}
		
		try {
			
			//Delete(Move to Recycle Object Children)
			dmu.deleteObject(object);
			
			//Result
			result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_INFO, EIMCommandResultConstant.NONE, EIMCommandResultConstant.MSG_OBJECT_DELETED + "(" + path + ")");
			
			//Commit
			sess.commit();
			
		} catch(Exception e) {
			
			//Rollback
			sess.rollback();
			
			//Result
			result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR, EIMCommandResultConstant.CODE_SYSTEM_ERROR, e.getMessage() + "(" + path + ")");
			
		} finally {
			sess.close();
		}
		
		return result;
		
	}
	
}
