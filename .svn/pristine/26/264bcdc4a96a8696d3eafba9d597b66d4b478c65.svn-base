package eimtest.app.util;

import java.io.File;
import java.io.FileNotFoundException;
import java.net.InetAddress;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.ArrayUtils;

import common.util.AppConstant;
import common.util.AppObjectUtil;

import eim.bo.EIMAccessEntry;
import eim.bo.EIMAccessEntryType;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelationType;
import eim.bo.EIMSecurity;
import eim.bo.EIMStatusType;
import eim.bo.EIMUser;
import eim.bo.EIMValueType;
import eim.bo.EIMWorkFlow;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.FileUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eim.util.SecurityUtils;
import eim.util.StringUtils;
import eim.util.UserUtils;
import eim.util.WorkFlowUtils;
import eimtest.util.TestDBUtil;
import eimtest.util.TestSessionUtil;

/** */
public class CreateFolderTreeData {
	/** */
	boolean attrAssignToWorkspaceF;

	/** */
	EIMSession sess;

	/** */
	XlsSheetAccess xls;

	/** */
	ObjectTypeEnum objTypes;

	/** */
	EIMRelationType relType;

	/** */
	EIMAttributeType attrTypePath;

	/** */
	EIMAttributeType attrTypeHigherWFFolder;

	/** */
	EIMAttributeType attrTypeNameAttr;

	/** */
	EIMAttributeType attrTypeToLowAttr;

	/** */
	EIMAttributeType attrTypeFromHighAttr;

	/** */
	EIMWorkFlow wf;

	/** */
	EIMFormat fmt;

	/** */
	EIMFormat pfmt;

	/** */
	SheetDef def;

	/** */
	EIMSecurity sec;

	/** */
	EIMUser[] users;

	/** */
	List objs = new ArrayList();

	/** */
	String serial = InetAddress.getLocalHost().getHostName() + "-"
			+ new SimpleDateFormat("yyyyMMdd_HHmmss_SSS").format(new Date()) + "-";

	/** */
	static final String OBJTYPE_DOC = "ツリーテスト用ドキュメント";

	/** */
	static final String OBJTYPE_DOC_WF = "ツリーテスト用ドキュメントWF";

	/** */
	static final String OBJTYPE_FOLDER = "ツリーテスト用フォルダ";

	/** */
	static final String OBJTYPE_FOLDER_WF = "ツリーテスト用フォルダWF";

	/** */
	static final String OBJTYPE_WS = "ワークスペース";

	/** */
	static final String WF1 = "ツリーテスト用ワークフロー";

	/** */
	static final String SEC1 = "ツリーテスト用セキュリティ";

	/** */
	static final String FMT1 = "一時フォーマット";

	/** */
	static final String FMT1_PATH = "/export/home/eim/canonrm4work/";

	/**
	 * 
	 * @param args
	 * @throws Exception
	 */
	public static void main(String[] args) throws Exception {
		EIMSession sess = TestSessionUtil.createEIMSession();
		List argList = new ArrayList(Arrays.asList(args));
		boolean attrAssignToWorkspaceF = false;
		if (argList.size() > 0 && argList.get(0).equals("-attrAssignToWorkspace")) {
			attrAssignToWorkspaceF = true;
			argList.remove(0);
		}
		new CreateFolderTreeData(sess, (argList.size() > 0) ? (String) argList.get(0) : null,
				attrAssignToWorkspaceF).process();
		sess.commit();
	}

	/**
	 * 
	 * @param sess
	 * @param xlsFileName
	 * @throws Exception
	 */
	public CreateFolderTreeData(EIMSession sess, String xlsFileName) throws Exception {
		this(sess, xlsFileName, false);
	}

	/**
	 * 
	 * @param sess
	 * @param xlsFileName
	 * @param attrAssignToWorkspaceF
	 * @throws Exception
	 */
	public CreateFolderTreeData(EIMSession sess, String xlsFileName, boolean attrAssignToWorkspaceF)
			throws Exception {
		this.attrAssignToWorkspaceF = attrAssignToWorkspaceF;

		if (xlsFileName == null)
			xlsFileName = "/SimpleFolderTreeWithWF.xls";

		if (!new File(xlsFileName).exists()) {
			URL url = this.getClass().getResource(xlsFileName);
			if (url == null) {
				this.getClass().getClassLoader().getResource(xlsFileName);
				if (url == null)
					throw new FileNotFoundException(xlsFileName);
			}
			xlsFileName = url.getPath();
		}
		this.sess = sess;
		System.out.println(" load from " + xlsFileName);
		xls = new XlsSheetAccess(xlsFileName);
	}

	/**
	 * 
	 * @throws Exception
	 */
	void init() throws Exception {
		objTypes = new ObjectTypeEnum(sess);
		relType = RelationUtils.getRelationTypeByName(sess, "ドキュメント");
		attrTypePath = AttributeUtils.getAttributeTypeByName(sess, "パス");
		attrTypeHigherWFFolder = AttributeUtils.getAttributeTypeByName(sess, "上位WFフォルダ");
		attrTypeNameAttr = AttributeUtils.getAttributeTypeByName(sess, "名称割当て属性");
		attrTypeToLowAttr = AttributeUtils.getAttributeTypeByName(sess, "下位への引継ぎ属性");
		attrTypeFromHighAttr = AttributeUtils.getAttributeTypeByName(sess, "上位からの引継ぎ属性");
		def = parseDef(xls);

		initUser();
		initWF();
		initFmt();
		initType();
		initAttr();
		initSecurity();

		pfmt = FileUtils.getFormatByName(sess, "公開ドキュメント");
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void process() throws Exception {
		System.out.println(this + " " + TestDBUtil.getConData(sess.getDBConnection()));
		init();
		while (findNext()) {
			String objTypeName = null;
			SheetDefOfObj objDef = new SheetDefOfObj(xls.get());
			String objName = objDef.name;
			boolean isTypeOfDocument = false;
			boolean isObjnameStartSlash = false;
			if (objName.startsWith("/")) {
				isObjnameStartSlash = true;
				objName = objName.substring(1);
			}

			if (xls.lastCol == def.tree.treeColSt) {
				objTypeName = OBJTYPE_WS;
				objName = serial + objName;
			} else if (isObjnameStartSlash)
				objTypeName = objDef.hasWf ? OBJTYPE_FOLDER_WF : OBJTYPE_FOLDER;
			else {
				objTypeName = objDef.hasWf ? OBJTYPE_DOC_WF : OBJTYPE_DOC;
				isTypeOfDocument = true;
			}

			EIMObject parentObj = findParent();

			List attrs = new ArrayList();
			for (int i = 0; i < def.attrs.length; i++) {
				SheetDefOfAttr defAttr = def.attrs[i];
				String val = xls.get(-1, defAttr.col);
				if (val.length() > 0) {
					Boolean doSyncWithName = Boolean.FALSE;
					Boolean doSyncBelowObject = Boolean.FALSE;
					if (val.indexOf('[') >= 0) {
						if (isTypeOfDocument)
							throw new IllegalArgumentException(
									"attribute option '[～～]' is not support to document type. row=:"
											+ xls.lastRow + ",col=" + xls.lastCol + ",value=" + val);
						String opt = val.replaceAll(".*\\[", "");
						val = val.replaceAll("\\[.*\\]", "");
						doSyncWithName = Boolean.valueOf(opt.indexOf('←') >= 0);
						doSyncBelowObject = Boolean.valueOf(opt.indexOf('↓') >= 0);
					}
					attrs.add(new Object[] { defAttr, val, doSyncWithName, doSyncBelowObject });
				}
			}
			EIMObject obj = createObject(objName, parentObj, objTypeName, attrs, objDef,
				isTypeOfDocument);
			objs.add(obj);
		}

		System.out.println(this + " " + "fin");
	}

	/**
	 * 
	 * @param name
	 * @param parentObj
	 * @param objTypeName
	 * @param attrs
	 * @param objDef
	 * @param isTypeOfDocument
	 * @return o
	 * @throws Exception
	 */
	EIMObject createObject(String name, EIMObject parentObj, String objTypeName, List attrs,
			SheetDefOfObj objDef, boolean isTypeOfDocument) throws Exception {
		EIMObject obj = ObjectUtils.createObject(sess, objTypes.get(objTypeName), name);
		if (parentObj != null)
			RelationUtils.createRelation(sess, relType, parentObj, obj);

		{
			String parentPath;
			if (parentObj != null) {
				parentPath = AppObjectUtil.getStrAttr(sess, parentObj, "パス");
				if (parentPath == null)
					parentPath = "/";
				ObjectAttributeUtils.setAttribute(sess, obj, attrTypePath, parentPath
						+ parentObj.getName() + "/");
			}
		}

		// 属性値を作成する
		List attrToLowAttrs = new ArrayList();
		for (Iterator i = attrs.iterator(); i.hasNext();) {
			Object[] attrDef = (Object[]) i.next();
			SheetDefOfAttr defAttr = (SheetDefOfAttr) attrDef[0];
			String value = (String) attrDef[1];
			boolean doSyncWithName = ((Boolean) attrDef[2]).booleanValue();
			boolean doSyncBelowObject = ((Boolean) attrDef[3]).booleanValue();

			// 下位引き継ぎ属性のリストをここでは生成
			if (doSyncBelowObject) {
				attrToLowAttrs.add(new Integer(defAttr.attrType.getId()));
			}

			// 名称割り当て属性
			if (doSyncWithName) {
				ObjectAttributeUtils.setAttribute(sess, obj, attrTypeNameAttr,
					defAttr.attrType.getId());
				ObjectAttributeUtils.setAttribute(sess, obj, defAttr.attrType, name);
				continue;
			}

			// 値
			switch (defAttr.valTypeId) {
			case EIMValueType.INTEGER:
				if (defAttr.isMultivalue) {
					String[] values = value.split(",");
					int[] intValues = new int[values.length];
					for (int j = 0; j < values.length; j++) {
						intValues[j] = Integer.parseInt(values[j]);
					}
					ObjectAttributeUtils.setAttribute(sess, obj, defAttr.attrType, intValues);
				} else {
					ObjectAttributeUtils.setAttribute(sess, obj, defAttr.attrType,
						Integer.parseInt(value));
				}
				break;
			case EIMValueType.DATE:
				if (defAttr.isMultivalue) {
					String[] values = value.split(",");
					Date[] dtValues = new Date[values.length];
					for (int j = 0; j < values.length; j++) {
						dtValues[j] = StringUtils.getDateFromString(values[j]);
					}
					ObjectAttributeUtils.setAttribute(sess, obj, defAttr.attrType, dtValues);
				} else {
					ObjectAttributeUtils.setAttribute(sess, obj, defAttr.attrType,
						StringUtils.getDateFromString(value));
				}
				break;
			case EIMValueType.STRING:
			case EIMValueType.TEXT:
				if (defAttr.isMultivalue) {
					String[] values = value.split(",");
					ObjectAttributeUtils.setAttribute(sess, obj, defAttr.attrType, values);
				} else {
					ObjectAttributeUtils.setAttribute(sess, obj, defAttr.attrType, value);
				}
				break;
			}
		}

		// parentObjの下位への引継ぎ属性を「引き継ぐ」
		int[] parentsCopyAttrs = null;
		if (parentObj != null) {
			parentsCopyAttrs = AppObjectUtil.getIntAttrs(sess, parentObj,
				attrTypeToLowAttr.getName());
			if (parentsCopyAttrs == null)
				parentsCopyAttrs = new int[0];
			ObjectAttributeUtils.setAttribute(sess, obj, attrTypeFromHighAttr, parentsCopyAttrs);

			for (int i = 0; i < parentsCopyAttrs.length; i++) {
				EIMAttributeType attrType = null;
				for (int j = 0; j < def.attrs.length; j++) {
					if (def.attrs[j].attrType.getId() == parentsCopyAttrs[i]) {
						attrType = def.attrs[j].attrType;
						break;
					}
				}
				if (attrType == null)
					throw new IllegalStateException("not exist copy attrType.attrType="
							+ parentsCopyAttrs[i]);
				switch (attrType.getValueType().getId()) {
				case EIMValueType.INTEGER:
					ObjectAttributeUtils.setAttribute(sess, obj, attrType,
						AppObjectUtil.getIntAttrs(sess, parentObj, attrType.getName()));
					break;
				case EIMValueType.DATE:
					ObjectAttributeUtils.setAttribute(sess, obj, attrType,
						AppObjectUtil.getDateAttrs(sess, parentObj, attrType.getName()));
					break;
				case EIMValueType.STRING:
					ObjectAttributeUtils.setAttribute(sess, obj, attrType,
						AppObjectUtil.getStrAttrs(sess, parentObj, attrType.getName()));
					break;
				case EIMValueType.TEXT:
					ObjectAttributeUtils.setAttribute(sess, obj, attrType,
						AppObjectUtil.getTextAttrs(sess, parentObj, attrType.getName()));
					break;
				}
			}
		}

		// 下位引継ぎ属性
		if (!isTypeOfDocument) {
			if (parentsCopyAttrs != null)
				attrToLowAttrs.addAll(Arrays.asList(ArrayUtils.toObject(parentsCopyAttrs)));
			if (attrToLowAttrs.size() > 0) {
				ObjectAttributeUtils.setAttribute(
					sess,
					obj,
					attrTypeToLowAttr,
					ArrayUtils.toPrimitive((Integer[]) attrToLowAttrs.toArray(new Integer[attrToLowAttrs.size()])));
			}
		}

		// ステータス
		if (parentObj != null && parentObj.getStatus() != null) {
			WorkFlowUtils.updateObjectStatus(sess, obj, parentObj.getStatus());
			EIMAttribute attrHigherWFFolder = parentObj.getAttribute(attrTypeHigherWFFolder.getDefName());
			int attrHigherWFFolderId = (attrHigherWFFolder != null) ? attrHigherWFFolder.getInt()
					: parentObj.getId();
			ObjectAttributeUtils.setAttribute(sess, obj, attrTypeHigherWFFolder,
				attrHigherWFFolderId);
		} else {
			// ステータスを進める
			for (int i = 0; i < objDef.statusStepUp; i++) {
				WorkFlowUtils.statusUp(sess, obj);
			}
		}

		// checkin file
		if (isDocumentType(objTypeName)) {
			FileUtils.checkin(sess, obj, fmt, name, (long) Math.random() * 10000);

			// public file
			obj = ObjectUtils.getObjectById(sess, obj.getId());
			if (obj.getStatus() == null
					|| obj.getStatus().getType().getKind() == AppConstant.STATUS_TYPE_KIND_ID_PUBLIC) {
				FileUtils.checkin(sess, obj, pfmt, name.replaceAll("\\..*", "") + ".pdf",
					(long) Math.random() * 10000);
			}
		}

		// security
		SecurityUtils.setSecurity(sess, obj, sec);

		return ObjectUtils.getObjectById(sess, obj.getId());
	}

	/**
	 * 
	 * @throws Exception
	 */
	void initFmt() throws Exception {
		try {
			File dir = new File(FMT1_PATH);
			if (dir.exists()) {
				dir.renameTo(new File(dir.getAbsolutePath() + ".bk" + new Date().getTime()));
			}
			FileUtils.createFormat(sess, FMT1, FMT1_PATH);
		} catch (EIMException e) {
			if (e.getCode() != 502)
				throw e;
		}
		fmt = FileUtils.getFormatByName(sess, FMT1);
	}

	/**
	 * 
	 * @throws Exception
	 */
	void initUser() throws Exception {
		users = new EIMUser[4];
		for (int i = 1; i <= 4; i++) {
			String userCd = "tu" + i;
			try {
				UserUtils.createUser(sess, userCd, userCd, userCd, userCd, userCd, 0);
			} catch (EIMException e) {
				if (e.getCode() != 1701)
					throw e;
			}
			users[i - 1] = UserUtils.getUserByCode(sess, userCd);
		}
	}

	/**
	 * 
	 * @throws Exception
	 */
	void initSecurity() throws Exception {
		sec = TestAppSecurityUtil.setEntry(sess, SEC1, users, new int[] { 0, 1, 2, 3 });
	}

	/**
	 * 
	 * @throws Exception
	 */
	void initWF() throws Exception {
		try {
			wf = WorkFlowUtils.createWorkFlow(sess, WF1);
			WorkFlowUtils.createStatusType(sess, wf, "編集中", AppConstant.STATUS_TYPE_KIND_ID_EDITTING);

			EIMStatusType stt = WorkFlowUtils.createStatusType(sess, wf, "承認依頼中",
				AppConstant.STATUS_TYPE_KIND_ID_APPROVE);
			EIMAccessEntry ent = SecurityUtils.createAccessEntry(sess, stt, new EIMAccessEntryType(
					EIMAccessEntryType.USER), UserUtils.getUserByCode(sess, "tu1"));
			SecurityUtils.updateAccessRole(sess, ent, new EIMAccessRole(EIMAccessRole.APPROVE), 1);

			stt = WorkFlowUtils.createStatusType(sess, wf, "公開処理中",
				AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC);
			AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFPUB"),
				String.valueOf(stt.getId()));

			WorkFlowUtils.createStatusType(sess, wf, "公開中", AppConstant.STATUS_TYPE_KIND_ID_PUBLIC);

			// Create Work Flow Setting Object
			EIMObject wfSetting = AppObjectUtil.createObject(sess,
				EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"), String.valueOf(wf.getId()));
			AppObjectUtil.setAttr(sess, wfSetting,
				EIMConfig.get("ATTR_NAME_WFSETTING_MAIL_NOTICE_DFLT"), "off");
			AppObjectUtil.setAttr(sess, wfSetting,
				EIMConfig.get("ATTR_NAME_WFSETTING_SETTING_FLG"), 1);
			AppObjectUtil.setAttr(sess, wfSetting,
				EIMConfig.get("ATTR_NAME_WFSETTING_POPUP_NOTICE_FLG"), 0);
			AppObjectUtil.setAttr(sess, wfSetting,
				EIMConfig.get("ATTR_NAME_WFSETTING_MAIL_NOTICE_FLG"), 0);
		} catch (EIMException e) {
			if (e.getCode() != 1908)
				throw e;
		}
		List wfs = WorkFlowUtils.getWorkFlowList(sess);
		for (Iterator i = wfs.iterator(); i.hasNext();) {
			EIMWorkFlow wfi = (EIMWorkFlow) i.next();
			if (wfi.getName().equals(WF1)) {
				wf = wfi;
				break;
			}
		}
	}

	/**
	 * 
	 * @throws Exception
	 */
	void initType() throws Exception {
		try {
			ObjectUtils.createObjectType(sess, OBJTYPE_FOLDER, objTypes.get("フォルダ"));
		} catch (EIMException e) {
			if (e.getCode() != 908)
				throw e;
		}
		try {
			ObjectUtils.createObjectType(sess, OBJTYPE_FOLDER_WF, objTypes.get(OBJTYPE_FOLDER));
		} catch (EIMException e) {
			if (e.getCode() != 908)
				throw e;
		}
		EIMObjectType objType = objTypes.get(OBJTYPE_FOLDER_WF);
		WorkFlowUtils.applyWorkFlow(sess, objType, wf);

		try {
			ObjectUtils.createObjectType(sess, OBJTYPE_DOC, objTypes.get("ドキュメント"));
		} catch (EIMException e) {
			if (e.getCode() != 908)
				throw e;
		}
		objType = objTypes.get(OBJTYPE_DOC);
		try {
			FileUtils.applyFormat(sess, objType, fmt);
		} catch (EIMException e) {
			if (e.getCode() != 502 && e.getCode() != 506)
				throw e;
		}
		EIMFormat defaultFmt = FileUtils.getDefaultFormat(sess, objType);
		if (defaultFmt == null || defaultFmt.getId() != fmt.getId()) {
			FileUtils.setDefault(sess, objType, fmt);
		}

		try {
			ObjectUtils.createObjectType(sess, OBJTYPE_DOC_WF, objTypes.get(OBJTYPE_DOC));
		} catch (EIMException e) {
			if (e.getCode() != 908)
				throw e;
		}
		objType = objTypes.get(OBJTYPE_DOC_WF);
		WorkFlowUtils.applyWorkFlow(sess, objType, wf);
	}

	/**
	 * 
	 * @throws Exception
	 */
	void initAttr() throws Exception {
		for (int i = 0; i < def.attrs.length; i++) {
			SheetDefOfAttr defAttr = def.attrs[i];
			EIMAttributeType attrType = AttributeUtils.getAttributeTypeByName(sess, defAttr.name);
			if (attrType == null) {
				attrType = AttributeUtils.createAttributeType(sess, defAttr.name,
					EIMValueType.getTypeById(sess, defAttr.valTypeId), defAttr.isMultivalue);
				AttributeUtils.addOtherAttributeTypeName(sess, attrType.getId(), "JA", defAttr.name);
				AttributeUtils.addOtherAttributeTypeName(sess, attrType.getId(), "EN", "en:"
						+ defAttr.name);
			}
			defAttr.attrType = attrType;

			try {
				ObjectAttributeUtils.applyAttributeType(sess, objTypes.get(OBJTYPE_DOC), attrType);
			} catch (EIMException e) {
				if (e.getCode() != 210)
					throw e;
			}
			try {
				ObjectAttributeUtils.applyAttributeType(sess, objTypes.get(attrAssignToWorkspaceF
						? OBJTYPE_WS : OBJTYPE_FOLDER), attrType);
			} catch (EIMException e) {
				if (e.getCode() != 210)
					throw e;
			}
		}
	}

	/**
	 * 
	 * @return o
	 */
	EIMObject findParent() {
		if (xls.lastCol == 1)
			return null; // top level

		xls.pushMark();
		xls.prevCol();
		try {
			for (int offset = 1; xls.lastRow > 0; offset++) {
				if (xls.prevRow().get().length() > 0) {
					return (EIMObject) objs.get(objs.size() - offset);
				}
			}
		} finally {
			xls.popMark();
		}
		throw new IllegalStateException("can not find parent. row=" + xls.lastRow + ",col="
				+ xls.lastCol + ",name=" + xls.get());
	}

	/**
	 * 
	 * @return o
	 */
	boolean findNext() {
		xls.nextRow();
		for (int i = def.tree.treeColSt; i <= def.tree.treeColEnd; i++) {
			if (xls.seek(-1, i).get().length() > 0)
				return true;
		}
		return false;
	}

	/**
	 * 
	 * @param xls
	 * @return o
	 */
	SheetDef parseDef(XlsSheetAccess xls) {
		SheetDef ret = new SheetDef();
		xls.seek(1, 1).nextCol();
		for (; xls.get().length() == 0; xls.nextCol())
			;

		ret.tree = new SheetDefOfTree(1, xls.lastCol - 1);
		xls.nextCol();
		ret.nameCol = xls.lastCol;

		List attrs = new ArrayList();
		for (String attrName; (attrName = xls.get()).length() > 0; xls.nextCol()) {
			attrs.add(new SheetDefOfAttr(xls.lastCol, attrName));
		}
		ret.attrs = (SheetDefOfAttr[]) attrs.toArray(new SheetDefOfAttr[attrs.size()]);

		return ret;
	}

	/**
	 * 
	 * @param objTypeName
	 * @return o
	 */
	boolean isDocumentType(String objTypeName) {
		return (objTypeName == OBJTYPE_DOC) || (objTypeName == OBJTYPE_DOC_WF);
	}

	/** */
	class ObjectTypeEnum {
		/** */
		EIMSession sess;

		/** */
		Map typeMap = new HashMap();

		/**
		 * 
		 * @param sess
		 */
		ObjectTypeEnum(EIMSession sess) {
			this.sess = sess;
		}

		/**
		 * 
		 * @param objTypeName
		 * @return o
		 * @throws Exception
		 */
		EIMObjectType get(String objTypeName) throws Exception {
			EIMObjectType ret = (EIMObjectType) typeMap.get(objTypeName);
			if (ret != null)
				return ret;

			ret = ObjectUtils.getObjectTypeByName(sess, objTypeName);
			if (ret == null)
				throw new IllegalStateException("canot get objectType.type=" + objTypeName);
			typeMap.put(objTypeName, ret);
			return ret;
		}
	}

	/** */
	class AttrValue {
		/** */
		String attrName;

		/** */
		String value;
	}

	/** */
	class SheetDef {
		/** */
		SheetDefOfTree tree;

		/** */
		SheetDefOfAttr[] attrs;

		/** */
		int nameCol;
	}

	/** */
	class SheetDefOfTree {

		/** */
		int treeColSt;

		/** */
		int treeColEnd;

		/**
		 * 
		 * @param treeColSt
		 * @param treeColEnd
		 */
		SheetDefOfTree(int treeColSt, int treeColEnd) {
			this.treeColSt = treeColSt;
			this.treeColEnd = treeColEnd;
		}
	}

	/** */
	class SheetDefOfObj {
		/** */
		String name;

		/** */
		int statusStepUp = -1;

		/** */
		boolean hasWf;

		/** */
		static final int STATUS_STEPUP_PUBLIC = 3;

		/**
		 * 
		 * @param name
		 */
		SheetDefOfObj(String name) {
			this.name = name;
			int p = name.indexOf('[');
			if (p > 0) {
				this.name = name.substring(0, p);
				String statusKindStr = name.substring(p + 1, name.length() - 1);
				if (statusKindStr.equals("編集中")) {
					statusStepUp = 0;
				} else if (statusKindStr.equals("承認依頼中")) {
					statusStepUp = 1;
				} else if (statusKindStr.equals("公開処理中")) {
					statusStepUp = 2;
				} else if (statusKindStr.equals("公開中")) {
					statusStepUp = STATUS_STEPUP_PUBLIC;
				} else {
					throw new IllegalStateException("invalid status name:" + statusKindStr);
				}
			}
			hasWf = (statusStepUp >= 0);
		}
	}

	/** */
	class SheetDefOfAttr {
		/** */
		int col;

		/** */
		String name;

		/** */
		int valTypeId = EIMValueType.STRING;

		/** */
		boolean isMultivalue = false;

		/** */
		EIMAttributeType attrType;

		/**
		 * 
		 * @param col
		 * @param name
		 */
		SheetDefOfAttr(int col, String name) {
			this.col = col;
			this.name = name;
			if (name.indexOf(':') > 0) {
				String type = name.substring(name.indexOf(':') + 1);
				this.name = name.substring(0, name.indexOf(':'));
				if (type.equals("int")) {
					valTypeId = EIMValueType.INTEGER;
				} else if (type.equals("intN")) {
					valTypeId = EIMValueType.INTEGER;
					isMultivalue = true;
				} else if (type.equals("str")) {
					valTypeId = EIMValueType.STRING;
				} else if (type.equals("strN")) {
					valTypeId = EIMValueType.STRING;
					isMultivalue = true;
				} else if (type.equals("text")) {
					valTypeId = EIMValueType.TEXT;
				} else if (type.equals("textN")) {
					valTypeId = EIMValueType.TEXT;
					isMultivalue = true;
				} else if (type.equals("date")) {
					valTypeId = EIMValueType.DATE;
				} else if (type.equals("dateN")) {
					valTypeId = EIMValueType.DATE;
					isMultivalue = true;
				}
			}
		}
	}
}
