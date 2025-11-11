package app.document.senario;

import java.io.File;

import junit.framework.TestCase;

import org.apache.commons.lang.ClassUtils;

import app.document.DocumentService;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMUser;
import eim.bo.EIMSearchSelectEIMObject.PsedoAttributeTypeEnum;
import eim.bo.EIMSearchSelectEIMObject.SearchConditionBuildHelper;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.ObjectUtils;
import eim.util.SearchUtils;
import eim.util.UserUtils;
import eimtest.app.util.TestAppDBUtil;
import eimtest.app.util.XMLUtil;
import eimtest.app.util.net.SMTPListener;
import eimtest.app.util.net.SMTPListener.ReceivedMail;
import eimtest.util.TestSessionUtil;

/** */
public class ApproveMailTest extends TestCase {
	/** */
	DocumentService ds;

	/** */
	XMLUtil xu;

	/** */
	EIMSession sess;

	/** */
	EIMUser u1;

	/** */
	EIMUser u2;

	/** */
	EIMUser u3;

	/** */
	EIMUser u4;

	/** */
	SMTPListener smtpLstn;

	public void setUp() throws Exception
	{
		TestAppDBUtil.loadPrimitiveData();
		ds = new DocumentService("u1", "u1");
		xu = new XMLUtil();
		sess = TestSessionUtil.createEIMSession();
		u1 = UserUtils.getUserByCode(sess, "u1");
		u2 = UserUtils.getUserByCode(sess, "u2");
		u3 = UserUtils.getUserByCode(sess, "u3");
		u4 = UserUtils.getUserByCode(sess, "u4");
		smtpLstn = (SMTPListener)new SMTPListener().start();
	}

	protected void tearDown() throws Exception
	{
		smtpLstn.terminate();
		sess.close();
	}

	/**
	 * 
	 * @throws Exception
	 */
	public void testCreateDocumentAndMore() throws Exception
	{
		// login is 'u1'
		// create document for request wf
		File file = new File(this.getClass().getResource(
				ClassUtils.getShortClassName(this.getClass()) + ".class").getPath());
		EIMObjectType docType = ObjectUtils.getObjectTypeByName(sess, "ドキュメント");
		// create document ,creator as "u2"
		ds.object_actCreateDocument("F1", docType, "pp"//
				, u2.getId()//
				, "", file);
		EIMSearchSelectEIMObject s = new EIMSearchSelectEIMObject();
		SearchConditionBuildHelper h = new SearchConditionBuildHelper();
		s.setCondition(h.group(h.opAnd())//
				.addCondition(
						h.eq(h.opAnd(), AttributeUtils.getAttributeTypeByName(sess, "パス"),
								"/WS1/F1/"))//
				.addCondition(h.eq(h.opAnd(), PsedoAttributeTypeEnum.NAME, file.getName())//
				));
		EIMObject docObj = (EIMObject) SearchUtils.searchObjects(sess, s, null).get(0);
		assertEquals("u1", docObj.getCreateUser().getCode());
		assertEquals(u2.getId(), docObj.getAttribute("作成者").getInt());

		// @Test:mail request approve .requested by user 'u3'
		smtpLstn.resetQueue();
		ds.switchUser("u3");
		ds.approve_actRequestApprove(docObj.getId(), "1:" + u4.getId(), new String[] { "1:"
				+ u4.getId() }, "approve as fast as!", false, true);
		ReceivedMail mail = smtpLstn.popQueue();
		assertEquals("[u4@eim.com>]", mail.reciptTos.toString());
		assertTrue(mail.body.indexOf("下記ドキュメントの承認依頼が届いております。") >= 0);
		assertTrue(mail.body.indexOf("依頼者　　：　ja:u3") >= 0);
		assertTrue(mail.body.indexOf("Request User : en:u3") >= 0);

		// @Test:mail cancel request approve .
		smtpLstn.resetQueue();
		ds.switchUser("u3");
		ds.approve_actCancelRequestApprove(docObj.getId());
		mail = smtpLstn.popQueue();
		assertEquals("[u4@eim.com>]", mail.reciptTos.toString());
		assertTrue(mail.body.indexOf("下記ドキュメントは ja:u3 さん に承認依頼を取り消されました。") >= 0);
		assertTrue(mail.body.indexOf("作成者　　：　ja:u2") >= 0);
		assertTrue(mail.body.indexOf("Create User : en:u2") >= 0);

		// re request approve
		ds.switchUser("u3");
		ds.approve_actRequestApprove(docObj.getId(), "1:" + u4.getId(), new String[] { "1:"
				+ u4.getId() }, "approve as fast as!", false, true);

		// @Test:mail sendback .rejected by user 'u4'
		smtpLstn.resetQueue();
		ds.switchUser("u4");
		docObj = (EIMObject) SearchUtils.searchObjects(sess, s, null).get(0);
		ds.approve_actApprove(docObj, "back", u4.getId(), "", true, false);
		mail = smtpLstn.popQueue();
		assertEquals("[u3@eim.com>]", mail.reciptTos.toString());
		assertTrue(mail.body.indexOf("下記ドキュメントは ja:u4 さん に差戻しされました。") >= 0);
		assertTrue(mail.body.indexOf("作成者　　：　ja:u2") >= 0);
		assertTrue(mail.body.indexOf("Create User : en:u2") >= 0);

		// re request approve
		// @Test:mail request approve .requested by user 'u3'
		smtpLstn.resetQueue();
		ds.switchUser("u3");
		ds.approve_actRequestApprove(docObj.getId(), "1:" + u4.getId(), new String[] { "1:"
				+ u1.getId() }, "approve as fast as!", false, true);
		mail = smtpLstn.popQueue();
		assertEquals("[u4@eim.com>]", mail.reciptTos.toString());
		assertTrue(mail.body.indexOf("下記ドキュメントの承認依頼が届いております。") >= 0);
		assertTrue(mail.body.indexOf("依頼者　　：　ja:u3") >= 0);

		// @Test:mail approve .approved by user 'u4'
		smtpLstn.resetQueue();
		ds.switchUser("u4");
		docObj = (EIMObject) SearchUtils.searchObjects(sess, s, null).get(0);
		ds.approve_actApprove(docObj, "approve", u4.getId(), "", true, false);
		mail = smtpLstn.popQueue();
		assertEquals("[u3@eim.com>]", mail.reciptTos.toString());
		assertTrue(mail.body.indexOf("下記ドキュメントは ja:u4 さん に承認されました。") >= 0);
		assertTrue(mail.body.indexOf("作成者　　：　ja:u2") >= 0);
		assertTrue(mail.body.indexOf("Create User : en:u2") >= 0);

		// @Test:mail publiced
		mail = smtpLstn.popQueue();
		assertEquals("[u1@eim.com>]", mail.reciptTos.toString());
		assertTrue(mail.body.indexOf("下記ドキュメントの公開通知が届いております。") >= 0);
		assertTrue(mail.body.indexOf("作成者　　：　ja:u2") >= 0);
		assertTrue(mail.body.indexOf("Create User : en:u2") >= 0);

		// @Test:mail reply
		smtpLstn.resetQueue();
		ds.switchUser("u1");
		ds.user_actSendReplyMail(docObj.getId());
		mail = smtpLstn.popQueue();
		assertEquals("[u3@eim.com>]", mail.reciptTos.toString());
		assertTrue(mail.body.indexOf("下記ドキュメントは ja:u1 さんに参照されました。") >= 0);
		assertTrue(mail.body.indexOf("作成者　　：　ja:u2") >= 0);
		assertTrue(mail.body.indexOf("Create User   : en:u2") >= 0);
		assertNull(smtpLstn.popQueue());
	}

}
