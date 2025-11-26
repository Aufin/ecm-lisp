import { parse } from 'node-html-parser';
import  { default as iconvLite } from 'iconv-lite';
import { deEncapsulateSync } from 'rtf-stream-parser';
import { decompressRTF } from '@kenjiuno/decompressrtf';
import { escapeHTML } from '@wordpress/escape-html'
import fs from 'fs'
import imageType from 'image-type';

  export const msgInternalHtml = (msg) => {
    msg.parserConfig = msg.parserConfig || {};
    const msgInfo = msg.getFileData();
    const rtfb = msgInfo.compressedRtf
          ? Buffer.from(decompressRTF(msgInfo.compressedRtf))
          : false,
          rtf = rtfb
          ? deEncapsulateSync(rtfb, { decode: iconvLite.decode })
          : false;
    return rtf && rtf.mode === 'html' ? rtf.text
      : '<pre>' + msgInfo.body + '</pre>'
  }

  export const msgInternalDoc = (msg) => {
    const str = msgInternalHtml(msg)
    return parse(str);
  };

  export const msgDocInlineImgs = (msg, doc) => {
    return doc.querySelectorAll('img');
  }

  export const inlineCidImg = async (msg, img) => {
    const src = img.getAttribute('src')
    if (! src.startsWith('cid:')) {
      return img
    }
    const decoder = new TextDecoder('utf8');
    const cid = src.substring(4),
          atto = msg.getFileData().attachments.find(i => i.pidContentId === cid),
          content = !atto ? false
          : msg.getAttachment(atto).content,
          type = await imageType(content),
          b64 = Buffer.from(content).toString('base64'),
          newSrc = `data:${type.mime};base64,${b64}`

    img.setAttribute('src', newSrc);

    return img
  }

export const msgBodyHtml = async (msg) => {
   const ihtml = msgInternalHtml(msg),
         doc = msgInternalDoc(msg),
         imgs = msgDocInlineImgs(msg, doc);

  // console.log('have imgs', imgs.map(i => i.getAttribute('src')));

  await Promise.all(
    imgs.map(i => inlineCidImg(msg, i))
  );

  // console.log('have imgs', imgs.map(i => i.getAttribute('src')));
  return doc.outerHTML;
}

export const msgHeadersHtml = (msg) => {
	const data = msg.getFileData(),
		  { senderName, senderEmail, inetAcctName,
			senderAddressType,
			subject, recipients,
			messageDeliveryTime,
			lastModificationTime
		  } = data,
		  getSenderEmail = () => {
			  if (senderAddressType == "EX" ||
				  senderEmail.toLowerCase().startsWith('/o=')) {
				  return inetAcctName
			  }
			  return senderEmail
		  },
		  getToCc = () => {
			  return recipients.map(r => {
				  return `<b>${r.name}</b> &lt;${r.smtpAddress}&gt;`			  }).join(', ')

		  }
								   
			  

	return `<table>
 <tr>
     <td>Subject:</td>
     <td><b>${subject}</b></td>
  </tr>
 <tr>
     <td>From:</td>
     <td><b>${senderName}</b> &lt;${getSenderEmail()}&gt;</td>
  </tr>
 <tr>
     <td>To/cc:</td>
     <td>${getToCc()}</td>
  </tr>
 <tr>
     <td>Date:</td>
     <td><i>${messageDeliveryTime || lastModificationTime}</i></td>
  </tr>
  </table>
`
}

export const msgToHtml = async (msg) => {
	const head = msgHeadersHtml(msg),
		  body = await msgBodyHtml(msg)
	return `${head}
  <hr><br>
 ${body}` 
}
	
	
