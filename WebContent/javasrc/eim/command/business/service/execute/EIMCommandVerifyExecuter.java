package eim.command.business.service.execute;

import java.io.File;

import common.util.AppConstant;
import eim.bo.EIMAttributeType;
import eim.bo.EIMFile;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.command.business.service.EIMCommandService;
import eim.command.business.service.result.EIMCommandResult;
import eim.command.common.util.DocumentManagementUtils;
import eim.command.common.util.EIMCommandResultConstant;
import eim.command.common.util.EncryptService;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.FileUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.OperationHistoryUtils;

public class EIMCommandVerifyExecuter extends EIMCommandExecuter {
	private final String FORMAT_CODE_ORG			= "org";
	private final String FORMAT_CODE_PUBLIC		= "public";
	private final String FORMAT_CODE_SIGNED		= "signed";

	@Override
	public EIMCommandResult execute() throws Exception {

		//Session
		EIMSession sess = super.getSess();

		EIMCommandResult result = new EIMCommandResult();

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

		//Format
		EIMFormat format = FileUtils.getDefaultFormat(sess, object.getType());
		//EIMFormat format = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_SIGNENCR"));
		String formatName = super.getFormat();
		if(formatName != null && !formatName.equals("")) {
			if(formatName.equals(FORMAT_CODE_ORG)) {
				format = FileUtils.getDefaultFormat(sess, object.getType());
			} else if(formatName.equals(FORMAT_CODE_PUBLIC)) {
				format = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
			} else if(formatName.equals(FORMAT_CODE_SIGNED)) {
				format = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_SIGNENCR"));
			} else {
				result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR, EIMCommandResultConstant.CODE_FORMAT_NOT_FOUND, EIMCommandResultConstant.MSG_FORMAT_NOT_FOUND + "(" + formatName + ")");
				return result;
			}
		}
		if(format == null) {
			result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR, EIMCommandResultConstant.CODE_FORMAT_NOT_FOUND, EIMCommandResultConstant.MSG_FORMAT_NOT_FOUND);
			return result;
		}

		//Get File
		EIMFile file = FileUtils.getFile(sess, object, format);
		if(file == null) {
			result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR, EIMCommandResultConstant.CODE_FILE_NOT_CHECKIN, EIMCommandResultConstant.MSG_FILE_NOT_CHECKIN + "(" + format.getName() + ")");
			return result;
		}

		//Check File Ext
		if(!file.getExt().equalsIgnoreCase(".nlf")) {
			result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR, EIMCommandResultConstant.CODE_FILE_NOT_NLF, EIMCommandResultConstant.MSG_FILE_NOT_NLF + "(" + fullPath + ")");
			return result;
		}

		//Substance
		String serverPath = file.getDirectory().getPath() + FileUtils.getFileName(object, file);
		File substance = new File(serverPath);
		if(!substance.exists()) {
			result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR, EIMCommandResultConstant.CODE_FILE_NOT_FOUND, EIMCommandResultConstant.MSG_FILE_NOT_FOUND + "(" + format.getName() + ")");
			return result;
		}

		//Encrypt Service
		EncryptService es = new EncryptService(sess);

		//Verify
		int rscode = es.verify(substance);

		//Result
		if(rscode != 0) {
			result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR, EIMCommandResultConstant.CODE_NOT_SIGNED_DOCUMENT, EIMCommandResultConstant.MSG_NOT_SIGNED_DOCUMENT + "(" + fullPath + ")");
		} else {

			//Set Encrypted
			if(object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS")) == null || object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS")).getInt() != AppConstant.SIGNENCR_KIND_SIGNENCR) {
				setEncrypted(sess, object, format);
			}

			//Result Message
			result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_INFO, EIMCommandResultConstant.NONE, EIMCommandResultConstant.MSG_VERIFY_SUCCEEDED + "(" + fullPath + ")");

		}

		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, "3005", "72", EIMConstant.OBJECT, object, null, null, null, EIMCommandService.VERSION + ":" + fullPath);

		//Commit
		sess.commit();

		return result;

	}

	@SuppressWarnings("unchecked")
	private boolean setEncrypted(EIMSession sess, EIMObject object, EIMFormat format) throws Exception {

		//File
		EIMFile file = FileUtils.getFile(sess, object, format);

		//Substance
		File substance = new File(file.getDirectory().getPath() + FileUtils.getFileName(object, file));

		//Encrypt Service
		EncryptService es = new EncryptService(sess);

		//Set Signed Attribute
		EIMAttributeType attType = null;

		//Sign Status
		attType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"));
		ObjectAttributeUtils.setAttribute(sess, object, attType, AppConstant.SIGNENCR_KIND_SIGNENCR);

		//Sign Version
		String version = es.version();
		attType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_VER"));
		ObjectAttributeUtils.setAttribute(sess, object, attType, version);

		//Format Signed
		EIMFormat signedFormat = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_SIGNENCR"));

		//Checkin
		FileUtils.checkin(sess, object, signedFormat, file.getName(), substance.length());

		//Copy Substance for Signed Format
		FileUtils.copyFile(substance, new File(signedFormat.getDirectory().getPath() + substance.getName()));

		return true;

	}

}
