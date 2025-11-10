package eim.command.business.service.execute;

import java.text.SimpleDateFormat;
import java.util.Date;

import common.util.AppConstant;
import common.util.AppObjectUtil;
import common.util.AppUpdateNoticeUtils;
import common.util.SignUtil;
import eim.bo.EIMAccessRole;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.command.business.service.EIMCommandService;
import eim.command.business.service.result.EIMCommandResult;
import eim.command.common.util.DocumentManagementUtils;
import eim.command.common.util.EIMCommandResultConstant;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.ObjectUtils;
import eim.util.OperationHistoryUtils;
import eim.util.SecurityUtils;

public class EIMCommandEncExecuter extends EIMCommandExecuter {
	private final String	OPTION_ASYNC		= "async";
	private final int			SLEEP_TIME			= 1000;
	private final int			MAX_WAIT_COUNT	= 600;
	@Override
	public EIMCommandResult execute() throws Exception {
		//Session
		EIMSession sess = super.getSess();
		EIMCommandResult result = new EIMCommandResult();
		
		//Option
		String option = super.getOption();
		if(option != null && !option.equals(OPTION_ASYNC)) {
			
			result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR, EIMCommandResultConstant.MSG_SYSTEM_ERROR, EIMCommandResultConstant.MSG_INVALID_OPTION + "(" + option + ")");
			return result;
		}
		
		//Get Path
		String fullPath = super.getPath();
		
		//Directory Name and File Name
		String path = null;
		String name = null;
		if(fullPath.lastIndexOf("/") > 0) {
			path = fullPath.substring(0, fullPath.lastIndexOf("/") + 1);
			name = fullPath.substring(fullPath.lastIndexOf("/") + 1);
		} else {
			name = fullPath;
		}
		
		//Get Object
		DocumentManagementUtils dmu = new DocumentManagementUtils(sess);
		EIMObject object = dmu.getObjectByPathAndName(path, name);
		if(object == null) {
			result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR, EIMCommandResultConstant.CODE_OBJECT_NOT_FOUND, EIMCommandResultConstant.MSG_OBJECT_NOT_FOUND + "(" + fullPath + ")");
			return result;
		} else if(!dmu.isDocumentTypes(object.getType())) {
			result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR, EIMCommandResultConstant.CODE_OBJECT_NOT_DOCUMENT, EIMCommandResultConstant.MSG_OBJECT_NOT_DOCUMENT + "(" + fullPath + ")");
			return result;
		}
		
		//Check Signed Status
		long status = 0;
		if(object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS")) != null) {
			status = object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS")).getInt();
		}
		if(status == 1) {
			result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR, EIMCommandResultConstant.CODE_ALREADY_SIGNED, EIMCommandResultConstant.MSG_ALREADY_SIGNED + "(" + fullPath + ")");
			return result;
		} else if(status == 2) {
			result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR, EIMCommandResultConstant.CODE_SIGNING, EIMCommandResultConstant.MSG_SIGNING + "(" + fullPath + ")");
			return result;
		}
		
		//Check Update Authority
		if(object.getSecurity() != null) {
			if(!SecurityUtils.enabled(sess, object, sess.getUser(), EIMAccessRole.UPDATE)) {
				result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR, EIMCommandResultConstant.CODE_OBJECT_CREATE_NOT_ALLOWED, EIMCommandResultConstant.MSG_OBJECT_CREATE_NOT_ALLOWED + "(" + fullPath + ")");
				return result;
			}
		}
		
		//Check Ext
		if(!SignUtil.isSignEncrTarget(sess, object)) {
			result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR, EIMCommandResultConstant.CODE_SIGN_EXT_UNAVAILABLE, EIMCommandResultConstant.MSG_SIGN_EXT_UNAVAILABLE + "(" + fullPath + ")");
			return result;
		}
		
		//Object Type
		EIMObjectType objTypeSignEnc = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_SIGN_ENC"));
		
		//Execute Date
		SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss");
		String execDate = "_" + sdf.format(new Date());
		
		//Set Signed Status to Processing
		AppObjectUtil.setAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"), AppConstant.SIGNENCR_KIND_PROCESSING_SIGNENCR);
		
		//Create Sign Job Object
		EIMObject signJobObj = ObjectUtils.createObject(sess, objTypeSignEnc, sess.getUser().getId() + execDate);
		AppObjectUtil.setAttr(sess, signJobObj, EIMConfig.get("ATTR_NAME_SIGNENC_TARGET_DOC"), object.getId());
		AppObjectUtil.setAttr(sess, signJobObj, EIMConfig.get("ATTR_NAME_SIGNENC_OPERATOR"), sess.getUser().getId());
		
		// SearchFramework 検索FW更新通知 対象：対象ドキュメント
		AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_SIGNENCRYPT_DOCUMENT");
		
		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, "3004", "40", EIMConstant.OBJECT, object, null, null, null, EIMCommandService.VERSION + ":" + fullPath);
		
		//Commit
		sess.commit();
		
		//Check Async
		if(option != null && option.equals(OPTION_ASYNC)) {
			result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_INFO, EIMCommandResultConstant.NONE, EIMCommandResultConstant.MSG_VERIFY_REQUEST_RECEIVED + "(" + fullPath + ")");
		} else {
			
			//Wait For Sign Process
			int cnt = 0;
			while(true) {
				
				//Check Sign Status
				object = ObjectUtils.getObjectById(sess, object.getId());
				if(object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS")) != null) {
					status = object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS")).getInt();
				}
				if(status == 1) {
					result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_INFO, EIMCommandResultConstant.NONE, EIMCommandResultConstant.MSG_SIGN_SUCCEEDED + "(" + fullPath + ")");
					return result;
				} else if(status == 3) {
					result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR, EIMCommandResultConstant.CODE_SIGN_FAILED, EIMCommandResultConstant.MSG_SIGN_FAILED + "(" + fullPath + ")");
					return result;
				}
				
				//Check Timeout
				cnt++;
				if(cnt == MAX_WAIT_COUNT) {
					break;
				}
				Thread.sleep(SLEEP_TIME);
				
			}
			
			//Result
			result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR, EIMCommandResultConstant.CODE_SYSTEM_ERROR, EIMCommandResultConstant.MSG_SYSTEM_ERROR);
			
		}
		
		return result;
		
	}
	
}
