package jp.co.ctc_g.eim.app.document.common.aop.advice;

import java.util.Iterator;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;

import common.util.AppLogicUtil;
import common.util.AppLogicUtil.ProcessFolderTreeWalker;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import common.util.AppUpdateNoticeUtils;
import eim.bo.EIMAccessEntry;
import eim.bo.EIMAccessRole;
import eim.bo.EIMGroup;
import eim.bo.EIMObject;
import eim.bo.EIMRelation;
import eim.bo.EIMRole;
import eim.bo.EIMSecurity;
import eim.bo.EIMUser;
import eim.bo.EIMVersion;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.GroupUtils;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eim.util.RoleUtils;
import eim.util.SecurityUtils;
import eim.util.UserUtils;
import eim.util.VersionUtils;
import jp.co.ctc_g.eim.app.document.business.domain.DocumentDomain;
import jp.co.ctc_g.eim.app.document.business.domain.FolderDomain;
import jp.co.ctc_g.eim.app.document.business.domain.PlaceDomain;
import jp.co.ctc_g.eim.app.document.business.domain.WorkspaceDomain;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.domain.entity.GroupDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RoleDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.SecurityDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.UserDomain;

@Aspect
public class DocumentUpdateNoticeAdvice {

	private static Log log = LogFactory.getLog(DocumentUpdateNoticeAdvice.class);

	public void afterCreateWorkspace(JoinPoint jp, Object result) throws Exception {

		EIMSession sess = EIMThreadContext.getEIMSession();

		WorkspaceDomain workspace = (WorkspaceDomain) result;
		EIMObject w = ObjectUtils.getObjectById(sess, workspace.getId());

		AppUpdateNoticeUtils.updateNoticeInsert(w.getId(), "SEARCHFW_CREATE_WORKSPACE");

		return;

	}

	public void beforeUpdateWorkspace(JoinPoint jp) throws Exception {
		ObjectDomain workspace = (ObjectDomain) jp.getArgs()[0];
		EIMSession sess = EIMThreadContext.getEIMSession();

		// Workspace
		AppUpdateNoticeUtils.updateNoticeInsert(workspace.getId(), "SEARCHFW_UPDATE_WORKSPACE");
		EIMObject w = ObjectUtils.getObjectById(sess, workspace.getId());

		// セキュリティ変更
		if (workspace.getSecurity().getId() != w.getSecurity().getId()) {
			AppUpdateNoticeUtils.updateNoticeInsert(workspace.getId(), "SEARCHFW_SET_SECURITY_WORKSPACE");
			AppUpdateNoticeUtils.updateNoticeInsertChildRecursive(sess, w, "SEARCHFW_SET_SECURITY_CHILD_FOLDER",
					"SEARCHFW_SET_SECURITY_CHILD_DOCUMENT", "SEARCHFW_SET_SECURITY_CHILD_TAG", null);
		}

		// Rename
		if (!workspace.getName().equals(w.getName())) {
			AppUpdateNoticeUtils.updateNoticeInsertChildRecursive(sess, w,
					"SEARCHFW_UPDATE_ATTR_CHILD_FOLDER", "SEARCHFW_UPDATE_ATTR_CHILD_DOCUMENT",
					"SEARCHFW_UPDATE_ATTR_CHILD_TAG", "SEARCHFW_UPDATE_ATTR_CHILD_DOCUMENTLINK");
		}

		AppUpdateNoticeUtils.updateNoticeInsertChildRecursive(sess, w, "SEARCHFW_UPDATE_WORKSPACE_CHILD_FOLDER",
				"SEARCHFW_UPDATE_WORKSPACE_CHILD_DOCUMENT", "SEARCHFW_UPDATE_WORKSPACE_CHILD_TAG",
				"SEARCHFW_UPDATE_WORKSPACE_CHILD_LINK");

		return;
	}

	public void beforeDeleteWorkspace(JoinPoint jp) throws Exception {

		EIMSession sess = EIMThreadContext.getEIMSession();

		List<WorkspaceDomain> workspaces = (List<WorkspaceDomain>) jp.getArgs()[0];

		for (WorkspaceDomain workspace : workspaces) {

			EIMObject w = ObjectUtils.getObjectById(sess, workspace.getId());

			AppUpdateNoticeUtils.updateNoticeDelete(w.getId(), "SEARCHFW_DELETE_WORKSPACE");
		}

		return;

	}

	public void afterCreateDocument(JoinPoint jp, Object result) throws Exception {

		EIMSession sess = EIMThreadContext.getEIMSession();

		DocumentDomain document = (DocumentDomain) result;
		PlaceDomain place = (PlaceDomain) jp.getArgs()[1];
		EIMObject d = ObjectUtils.getObjectById(sess, document.getId());
		EIMObject p = ObjectUtils.getObjectById(sess, place.getId());

		AppUpdateNoticeUtils.updateNoticeInsert(d.getId(), "SEARCHFW_CREATE_DOCUMENT");
		AppUpdateNoticeUtils.updateNoticeInsertParent(sess, p, "SEARCHFW_CREATE_DOCUMENT_PARENT_FOLDER",
				"SEARCHFW_CREATE_DOCUMENT_PARENT_WORKSPACE", null);

		return;

	}

	public void afterUpdateDocument(JoinPoint jp) throws Exception {
		ObjectDomain document = (ObjectDomain) jp.getArgs()[0];

		// Document
		AppUpdateNoticeUtils.updateNoticeInsert(document.getId(), "SEARCHFW_UPDATE_ATTR_DOCUMENT");

		return;
	}

	public void beforeMoveDocument(JoinPoint jp) throws Exception {

		EIMSession sess = EIMThreadContext.getEIMSession();

		DocumentDomain document = (DocumentDomain) jp.getArgs()[0];
		EIMObject d = ObjectUtils.getObjectById(sess, document.getId());
		EIMObject[] parents = AppObjectUtil.getParentEIMObject(sess, d, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));

		// Parent Folder before Move
		if (parents.length > 0) {
			AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parents[0], "SEARCHFW_CUTPASTE_ORG_FOLDER",
					"SEARCHFW_CUTPASTE_ORG_WORKSPACE", null);
		}

		return;

	}

	public void afterMoveDocument(JoinPoint jp) throws Exception {

		EIMSession sess = EIMThreadContext.getEIMSession();

		DocumentDomain document = (DocumentDomain) jp.getArgs()[0];
		PlaceDomain place = (PlaceDomain) jp.getArgs()[1];
		EIMObject d = ObjectUtils.getObjectById(sess, document.getId());
		EIMObject p = ObjectUtils.getObjectById(sess, place.getId());

		// Document
		AppUpdateNoticeUtils.updateNoticeInsertObject(sess, d, "SEARCHFW_CUTPASTE_FOLDER", "SEARCHFW_CUTPASTE_DOCUMENT",
				"SEARCHFW_CUTPASTE_TAG", null, true);

		// Parent Folder after Move
		AppUpdateNoticeUtils.updateNoticeInsertParent(sess, p, "SEARCHFW_PASTE_DEST_FOLDER",
				"SEARCHFW_PASTE_DEST_WORKSPACE", null);

		return;

	}

	public void afterCheckout(JoinPoint jp, Object result) throws Exception {

		// EIMSession sess = EIMThreadContext.getEIMSession();

		DocumentDomain rev1 = (DocumentDomain) jp.getArgs()[0];
		DocumentDomain rev2 = (DocumentDomain) result;

		// Now Revision
		AppUpdateNoticeUtils.updateNoticeInsert(rev1.getId(), "SEARCHFW_CHECK_OUT_DOCUMENT");

		// New Revision
		AppUpdateNoticeUtils.updateNoticeInsert(rev2.getId(), "SEARCHFW_CHECK_OUT_NEW_DOCUMENT");

		return;

	}

	public void beforeCancelCheckout(JoinPoint jp) throws Exception {

		EIMSession sess = EIMThreadContext.getEIMSession();

		DocumentDomain document = (DocumentDomain) jp.getArgs()[0];
		EIMObject d = ObjectUtils.getObjectById(sess, document.getId());
		EIMVersion v = VersionUtils.getVersion(sess, d);
		EIMObject r = v.getLatest();

		// Now Revision
		AppUpdateNoticeUtils.updateNoticeInsert(d.getId(), "SEARCHFW_CANCEL_CHECK_OUT_DOCUMENT");

		// New Revision
		AppUpdateNoticeUtils.updateNoticeDelete(r.getId(), "SEARCHFW_CANCEL_CHECK_OUT_NEW_DOCUMENT");

		return;

	}

	public void afterCheckin(JoinPoint jp) throws Exception {

		EIMSession sess = EIMThreadContext.getEIMSession();

		DocumentDomain document = (DocumentDomain) jp.getArgs()[0];
		EIMObject d = ObjectUtils.getObjectById(sess, document.getId());

		// Revision > 0, and Workflow is not Applied
		if (d.getRev() > 0 && d.getStatus() == null) {

			// Previous Revision
			EIMVersion v = VersionUtils.getVersion(sess, d);
			EIMObject r = v.getObjectByRev(d.getRev() - 1);

			AppUpdateNoticeUtils.updateNoticeInsert(r.getId(), "SEARCHFW_CHECK_IN_OLD_DOCUMENT");

		}

		// Document
		AppUpdateNoticeUtils.updateNoticeInsert(document.getId(), "SEARCHFW_CHECK_IN_DOCUMENT");

		return;

	}

	public void afterReplaceOriginalFile(JoinPoint jp) throws Exception {

		DocumentDomain document = (DocumentDomain) jp.getArgs()[0];
		AppUpdateNoticeUtils.updateNoticeInsert(document.getId(), "SEARCHFW_CHECK_IN_DOCUMENT");

		return;

	}

	public void afterReplacePublicFile(JoinPoint jp) throws Exception {

		ObjectDomain document = (ObjectDomain) jp.getArgs()[0];
		AppUpdateNoticeUtils.updateNoticeInsert(document.getId(), "SEARCHFW_CHECK_IN_DOCUMENT");

		return;

	}

	public void beforeDisposeDocument(JoinPoint jp) throws Exception {

		EIMSession sess = EIMThreadContext.getEIMSession();

		List<DocumentDomain> documents = (List<DocumentDomain>) jp.getArgs()[0];
		for (DocumentDomain document : documents) {

			EIMObject d = ObjectUtils.getObjectById(sess, document.getId());
			EIMObject[] parents = AppObjectUtil.getParentEIMObject(sess, d,
					EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));

			if (parents.length > 0) {
				// Parent Folder
				AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parents[0], "SEARCHFW_LOGICDEL_PARENT_FOLDER",
						"SEARCHFW_LOGICDEL_PARENT_WORKSPACE", null);
			}
		}

		return;

	}

	public void afterDisposeDocument(JoinPoint jp) throws Exception {

		EIMSession sess = EIMThreadContext.getEIMSession();

		List<DocumentDomain> documents = (List<DocumentDomain>) jp.getArgs()[0];
		for (DocumentDomain document : documents) {

			EIMObject d = ObjectUtils.getObjectById(sess, document.getId());

			// Document
			AppUpdateNoticeUtils.updateNoticeInsertObject(sess, d, "SEARCHFW_LOGICDEL_DOCUMENT", true);

		}

		return;

	}

	public void beforeDeleteDocument(JoinPoint jp) throws Exception {

		EIMSession sess = EIMThreadContext.getEIMSession();
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		List<DocumentDomain> documents = (List<DocumentDomain>) jp.getArgs()[0];
		for (DocumentDomain document : documents) {

			EIMObject d = ObjectUtils.getObjectById(sess, document.getId());
			EIMVersion v = VersionUtils.getVersion(sess, d);

			for (Iterator j = v.getList().iterator(); j.hasNext();) {
				EIMObject r = (EIMObject) j.next();
				AppUpdateNoticeUtils.updateNoticeDelete(r.getId(), "SEARCHFW_PHYSICALDEL_DOCUMENT");
				AppUpdateNoticeUtils.updateNoticeInsertBranch(helper.getSession(), r,
						"SEARCHFW_PHYSICALDEL_ORG_BRANCH_DOCUMENT");
			}

		}

		return;

	}

	public void afterDeleteRevision(JoinPoint jp) throws Exception {

		List<DocumentDomain> documents = (List<DocumentDomain>) jp.getArgs()[0];
		for (DocumentDomain document : documents) {
			AppUpdateNoticeUtils.updateNoticeDelete(document.getId(), "SEARCHFW_PHYSICALDEL_DOCUMENT");
		}

		return;

	}

	public void afterCreateFolder(JoinPoint jp, Object result) throws Exception {

		EIMSession sess = EIMThreadContext.getEIMSession();

		FolderDomain folder = (FolderDomain) result;
		PlaceDomain place = (PlaceDomain) jp.getArgs()[1];
		EIMObject f = ObjectUtils.getObjectById(sess, folder.getId());
		EIMObject p = ObjectUtils.getObjectById(sess, place.getId());

		// Folder
		AppUpdateNoticeUtils.updateNoticeInsert(f.getId(), "SEARCHFW_CREATE_FOLDER");

		// parent Folder
		AppUpdateNoticeUtils.updateNoticeInsertParent(sess, p, "SEARCHFW_CREATE_FOLDER_PARENT_FOLDER",
				"SEARCHFW_CREATE_FOLDER_PARENT_WORKSPACE", null);

		return;

	}

	public void beforeUpdateFolder(JoinPoint jp) throws Exception {

		EIMSession sess = EIMThreadContext.getEIMSession();

		FolderDomain folder = (FolderDomain) jp.getArgs()[0];
		EIMObject f = ObjectUtils.getObjectById(sess, folder.getId());

		// Rename
		if (!folder.getName().equals(f.getName())) {
			AppUpdateNoticeUtils.updateNoticeInsertChildRecursive(sess, f, "SEARCHFW_RENAME_CHILD_FOLDER",
					"SEARCHFW_RENAME_CHILD_DOCUMENT", "SEARCHFW_RENAME_CHILD_TAG",
					"SEARCHFW_RENAME_CHILD_DOCUMENTLINK");
		}

		return;

	}

	public void afterUpdateFolder(JoinPoint jp) throws Exception {

		EIMSession sess = EIMThreadContext.getEIMSession();

		FolderDomain folder = (FolderDomain) jp.getArgs()[0];
		EIMObject f = ObjectUtils.getObjectById(sess, folder.getId());

		AppUpdateNoticeUtils.updateNoticeInsertObject(sess, f, "SEARCHFW_UPDATE_ATTR_FOLDER",
				"SEARCHFW_UPDATE_ATTR_DOCUMENT", "SEARCHFW_UPDATE_ATTR_TAG", "SEARCHFW_UPDATE_ATTR_WORKSPACE");

		return;

	}

	public void beforeMoveFolder(JoinPoint jp) throws Exception {

		EIMSession sess = EIMThreadContext.getEIMSession();

		FolderDomain folder = (FolderDomain) jp.getArgs()[0];
		EIMObject f = ObjectUtils.getObjectById(sess, folder.getId());
		EIMObject[] parents = AppObjectUtil.getParentEIMObject(sess, f, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));

		if (parents.length > 0) {
			// Parent Folder before Move
			AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parents[0], "SEARCHFW_CUTPASTE_ORG_FOLDER",
					"SEARCHFW_CUTPASTE_ORG_WORKSPACE", null);
		}

		return;

	}

	public void afterMoveFolder(JoinPoint jp) throws Exception {

		EIMSession sess = EIMThreadContext.getEIMSession();

		FolderDomain folder = (FolderDomain) jp.getArgs()[0];
		PlaceDomain place = (PlaceDomain) jp.getArgs()[1];
		EIMObject f = ObjectUtils.getObjectById(sess, folder.getId());
		EIMObject p = ObjectUtils.getObjectById(sess, place.getId());

		// Folder
		AppUpdateNoticeUtils.updateNoticeInsertObject(sess, f, "SEARCHFW_CUTPASTE_FOLDER", "SEARCHFW_CUTPASTE_DOCUMENT",
				"SEARCHFW_CUTPASTE_TAG", null, true);

		// Child Objects
		AppUpdateNoticeUtils.updateNoticeInsertChildRecursive(sess, f, "SEARCHFW_CUTPASTE_CHILD_FOLDER",
				"SEARCHFW_CUTPASTE_CHILD_DOCUMENT", "SEARCHFW_CUTPASTE_CHILD_TAG",
				"SEARCHFW_CUTPASTE_CHILD_DOCUMENTLINK");

		// Parent Folder after Move
		AppUpdateNoticeUtils.updateNoticeInsertParent(sess, p, "SEARCHFW_PASTE_DEST_FOLDER",
				"SEARCHFW_PASTE_DEST_WORKSPACE", null);

		log.info("afterMoveFolder fin");

		return;

	}

	public void beforeDisposeFolder(JoinPoint jp) throws Exception {

		EIMSession sess = EIMThreadContext.getEIMSession();

		List<FolderDomain> folders = (List<FolderDomain>) jp.getArgs()[0];
		for (FolderDomain folder : folders) {

			EIMObject f = ObjectUtils.getObjectById(sess, folder.getId());
			EIMObject[] parents = AppObjectUtil.getParentEIMObject(sess, f,
					EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));

			if (parents.length > 0) {
				// Parent Folder
				AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parents[0], "SEARCHFW_LOGICDEL_PARENT_FOLDER",
						"SEARCHFW_LOGICDEL_PARENT_WORKSPACE", null);
			}
		}

		return;

	}

	public void afterDisposeFolder(JoinPoint jp) throws Exception {

		EIMSession sess = EIMThreadContext.getEIMSession();

		List<FolderDomain> folders = (List<FolderDomain>) jp.getArgs()[0];
		for (FolderDomain folder : folders) {

			EIMObject f = ObjectUtils.getObjectById(sess, folder.getId());

			// Folder
			AppUpdateNoticeUtils.updateNoticeInsert(f.getId(), "SEARCHFW_LOGICDEL_FOLDER");

			// Child Objects
			AppUpdateNoticeUtils.updateNoticeInsertChildRecursive(sess, f, "SEARCHFW_LOGICDEL_CHILD_FOLDER",
					"SEARCHFW_LOGICDEL_CHILD_DOCUMENT", null, "SEARCHFW_LOGICDEL_CHILD_DOCUMENTLINK");

		}

		return;

	}

	public void beforeDeleteFolder(JoinPoint jp) throws Exception {

		EIMSession sess = EIMThreadContext.getEIMSession();
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		List<FolderDomain> folders = (List<FolderDomain>) jp.getArgs()[0];
		for (FolderDomain folder : folders) {

			EIMObject f = ObjectUtils.getObjectById(sess, folder.getId());

			// For Link
			AppUpdateNoticeUtils.updateNoticePhysicalDelLinkParent(sess, f, "SEARCHFW_PHYSICALDEL_LINK_FOLDER",
					"SEARCHFW_PHYSICALDEL_LINK_WORKSPACE");

			// Folder
			AppUpdateNoticeUtils.updateNoticeDelete(f.getId(), "SEARCHFW_PHYSICALDEL_FOLDER");

			ProcessFolderTreeWalker treeWalker = new ProcessFolderTreeWalker() {

				@Override
				public void walkIn(EIMObject upperFolderObjFrom, EIMObject lowerFolderObjTo,
						AppObjectConditionHelper helper) throws Exception {
				}

				@Override
				public void walkOut(EIMObject lowerFolderObjFrom, EIMObject upperFolderObjTo,
						AppObjectConditionHelper helper) throws Exception {
					List childRelList = RelationUtils.getChildRelationListByRelType(helper.getSession(),
							lowerFolderObjFrom, helper.getRelationTypeOfDocLink(), EIMAccessRole.READ);
					for (int i = 0; i < childRelList.size(); i++) {
						EIMRelation rel = (EIMRelation) childRelList.get(i);
						AppUpdateNoticeUtils.updateNoticeInsert(rel.getChild().getId(),
								"SEARCHFW_PHYSICALDEL_DOCUMENTLINK");

					}
					AppUpdateNoticeUtils.updateNoticeDelete(lowerFolderObjFrom.getId(), "SEARCHFW_PHYSICALDEL_FOLDER");
				}

				@Override
				public void processDocument(EIMObject docObj, AppObjectConditionHelper helper) throws Exception {
					AppUpdateNoticeUtils.updateNoticeDelete(docObj.getId(), "SEARCHFW_PHYSICALDEL_DOCUMENT");
					AppUpdateNoticeUtils.updateNoticeInsertBranch(helper.getSession(), docObj,
							"SEARCHFW_PHYSICALDEL_ORG_BRANCH_DOCUMENT");
				}

			};

			AppLogicUtil.processFolderTree(sess, f, true, false, treeWalker, helper);

		}

		return;
	}

	public void beforeUpdateUser(JoinPoint jp) throws Exception {
		// User
		UserDomain user = (UserDomain) jp.getArgs()[0];
		AppUpdateNoticeUtils.updateNoticeInsert(user.getId(), "SEARCHFW_USER_EDIT_USER");
		EIMSession sess = EIMThreadContext.getEIMSession();
		EIMUser u = UserUtils.getUserById(sess, user.getId());

		// User
		long userDisable = 0;

		if (!user.isDisable()){
			userDisable = 0;
		} else {
			userDisable = 1;
		}

		if ( userDisable != u.getDisable()) {
			List groups = GroupUtils.getGroupByUser(sess, u);
			for (int i = 0; i < groups.size(); i++) {
				EIMGroup group = (EIMGroup) groups.get(i);
				// SearchFramework 検索FW更新通知 対象：グループ
				AppUpdateNoticeUtils.updateNoticeInsert(group.getId(), "SEARCHFW_USER_EDIT_GROUP");
			}

			List roles = RoleUtils.getRoleByUser(sess, u);
			for (int j = 0; j < roles.size(); j++) {
				EIMRole role = (EIMRole) roles.get(j);
				// SearchFramework 検索FW更新通知 対象：ロール
				AppUpdateNoticeUtils.updateNoticeInsert(role.getId(), "SEARCHFW_USER_EDIT_ROLE");
			}

			// 全セキュリティを取得し、アクセスエントリのユーザを比較して対象のセキュリティを取得
			List<EIMSecurity> securityList = SecurityUtils.getSecurityList(sess);
			for (EIMSecurity security : securityList) {

				// 全セキュリティ取得
				List<EIMAccessEntry> accessEntryList = SecurityUtils.getAccessEntryList(sess, security);
				for (EIMAccessEntry accessEntry : accessEntryList) {

					// ユーザチェック
					if (accessEntry.getUser() == null) {
						continue;
					}
					if (accessEntry.getUser().getId() == user.getId()) {
						// SearchFramework 検索FW更新通知 対象：セキュリティ
						AppUpdateNoticeUtils.updateNoticeInsert(security.getId(),
								"SEARCHFW_SECURITY_DELACENTRY_SECURITY");
						break;
					}
				}
			}
		}
		return;
	}

	public void beforeDeleteUser(JoinPoint jp) throws Exception {

		UserDomain user = (UserDomain) jp.getArgs()[0];
		EIMSession sess = EIMThreadContext.getEIMSession();
		EIMUser u = UserUtils.getUserById(sess, user.getId());

		List groups = GroupUtils.getGroupByUser(sess, u);
		for (int i = 0; i < groups.size(); i++) {
			EIMGroup group = (EIMGroup)groups.get(i);
			// SearchFramework 検索FW更新通知 対象：グループ
			AppUpdateNoticeUtils.updateNoticeInsert(group.getId(), "SEARCHFW_USER_EDIT_GROUP");
		}

		List roles = RoleUtils.getRoleByUser(sess, u);
		for (int j = 0; j < roles.size(); j++) {
			EIMRole role = (EIMRole)roles.get(j);
			// SearchFramework 検索FW更新通知 対象：ロール
			AppUpdateNoticeUtils.updateNoticeInsert(role.getId(), "SEARCHFW_USER_EDIT_ROLE");
		}

		// 全セキュリティを取得し、アクセスエントリのユーザを比較して対象のセキュリティを取得
		List<EIMSecurity> securityList = SecurityUtils.getSecurityList(sess);
		for (EIMSecurity security : securityList) {

			// 全セキュリティ取得
			List<EIMAccessEntry> accessEntryList = SecurityUtils.getAccessEntryList(sess, security);
			for (EIMAccessEntry accessEntry : accessEntryList) {

				// ユーザチェック
				if (accessEntry.getUser() == null) {
					continue;
				}
				if (accessEntry.getUser().getId() == user.getId()) {
					// SearchFramework 検索FW更新通知 対象：セキュリティ
					AppUpdateNoticeUtils.updateNoticeInsert(security.getId(), "SEARCHFW_SECURITY_DELACENTRY_SECURITY");
					break;
				}
			}
		}
		return;
	}

	public void afterSetOtherNameUser(JoinPoint jp) throws Exception {
		UserDomain user = (UserDomain) jp.getArgs()[0];

		// User
		AppUpdateNoticeUtils.updateNoticeInsert(user.getId(), "SEARCHFW_USER_EDIT_USER");

		return;
	}

	public void afterRemoveOtherNameUser(JoinPoint jp) throws Exception {
		UserDomain user = (UserDomain) jp.getArgs()[0];

		// User
		AppUpdateNoticeUtils.updateNoticeInsert(user.getId(), "SEARCHFW_USER_EDIT_USER");

		return;
	}

	// GROUP

	public void afterUpdateGroup(JoinPoint jp) throws Exception {
		GroupDomain group = (GroupDomain) jp.getArgs()[0];

		// Group
		AppUpdateNoticeUtils.updateNoticeInsert(group.getId(), "SEARCHFW_USER_EDIT_GROUP");

		return;
	}

	public void afterAddUserGroup(JoinPoint jp) throws Exception {
		GroupDomain group = (GroupDomain) jp.getArgs()[0];

		// Group
		AppUpdateNoticeUtils.updateNoticeInsert(group.getId(), "SEARCHFW_USER_EDIT_GROUP");

		return;
	}

	public void afterRemoveUserGroup(JoinPoint jp) throws Exception {
		GroupDomain group = (GroupDomain) jp.getArgs()[0];

		// Group
		AppUpdateNoticeUtils.updateNoticeInsert(group.getId(), "SEARCHFW_USER_EDIT_GROUP");

		return;
	}

	public void afterSetOtherNameGroup(JoinPoint jp) throws Exception {
		GroupDomain group = (GroupDomain) jp.getArgs()[0];

		// Group
		AppUpdateNoticeUtils.updateNoticeInsert(group.getId(), "SEARCHFW_USER_EDIT_GROUP");

		return;
	}

	public void afterRemoveOtherNameGroup(JoinPoint jp) throws Exception {
		GroupDomain group = (GroupDomain) jp.getArgs()[0];

		// Group
		AppUpdateNoticeUtils.updateNoticeInsert(group.getId(), "SEARCHFW_USER_EDIT_GROUP");

		return;
	}

	public void afterUpdateRole(JoinPoint jp) throws Exception {
		RoleDomain role = (RoleDomain) jp.getArgs()[0];

		// Role
		AppUpdateNoticeUtils.updateNoticeInsert(role.getId(), "SEARCHFW_USER_EDIT_ROLE");

		return;
	}

	public void afterAddUserRole(JoinPoint jp) throws Exception {
		RoleDomain role = (RoleDomain) jp.getArgs()[0];

		// Role
		AppUpdateNoticeUtils.updateNoticeInsert(role.getId(), "SEARCHFW_USER_EDIT_ROLE");

		return;
	}

	public void afterRemoveUserRole(JoinPoint jp) throws Exception {
		RoleDomain role = (RoleDomain) jp.getArgs()[0];

		// Role
		AppUpdateNoticeUtils.updateNoticeInsert(role.getId(), "SEARCHFW_USER_EDIT_ROLE");

		return;
	}

	public void afterSetOtherNameRole(JoinPoint jp) throws Exception {
		RoleDomain role = (RoleDomain) jp.getArgs()[0];

		// Role
		AppUpdateNoticeUtils.updateNoticeInsert(role.getId(), "SEARCHFW_USER_EDIT_ROLE");

		return;
	}

	public void afterRemoveOtherNameRole(JoinPoint jp) throws Exception {
		RoleDomain role = (RoleDomain) jp.getArgs()[0];

		// Role
		AppUpdateNoticeUtils.updateNoticeInsert(role.getId(), "SEARCHFW_USER_EDIT_ROLE");

		return;
	}

	public void afterUpdateSecurity(JoinPoint jp) throws Exception {
		SecurityDomain security = (SecurityDomain) jp.getArgs()[0];

		// security
		AppUpdateNoticeUtils.updateNoticeInsert(security.getId(), "SEARCHFW_SECURITY_EDIT_SECURITY");

		return;
	}

	public void afterSetOtherNameSecurity(JoinPoint jp) throws Exception {
		SecurityDomain security = (SecurityDomain) jp.getArgs()[0];

		// security
		AppUpdateNoticeUtils.updateNoticeInsert(security.getId(), "SEARCHFW_SECURITY_EDIT_SECURITY");

		return;
	}

	public void afterRemoveOtherNameSecurity(JoinPoint jp) throws Exception {
		SecurityDomain security = (SecurityDomain) jp.getArgs()[0];

		// security
		AppUpdateNoticeUtils.updateNoticeInsert(security.getId(), "SEARCHFW_SECURITY_EDIT_SECURITY");

		return;
	}
}
