package no.hasmac.jsonld.uri;

import org.junit.jupiter.api.Test;

import java.net.URI;

import static org.junit.jupiter.api.Assertions.assertTrue;

class UriValidatorTest {

    String[] uriExtended = {
            "http://@:8080",
            "111https:&$good.com@good/good?ex=EX#FRAG",
            "http::::",
            "http://www.example.com#fragment?query",
            "http://hello#??someInvalidURi",
            "http:/google.com",
            "http://example.com:800080",
            "examples/http@:80",
            "http://host/%",
            "ht&tp://host.com",
            "http:/host",
            "/////a",
            "http:://host.com",
            "mailto:a@b@.com",
            "http://a_b.com",
            "ftp://host.com:65a/",
            "http://256.126.0.1",
            "http://.%0G",
            "htt p://example.com",
            "http:/example.com",
            "http0://example.com",
            ":::",
            "://.",
            "%xx",
            "http://example.com:-80",
            "http:user@info:com",
            " :://::@::443/path=%GFquery?#furtherrandomcharacters",
            "http://.",
            "1http://ww.example.com",  // scheme which starts with number
            "ht@tp://www.example.com",  // scheme with invalid characters
            "http://me@:80", // Malformed Authority - host is missing
            "http:://www.example.com",  // Multiple colons preventing proper scheme separation
            "http://",  // not checking for   empty host properly
            "http:///www.example.com", //  Triple '/' after scheme
            "http://www.example.com::80", // double colon before port
            ":http//www.example.com", // starting with special char
            "http://host:-80", // negative port number
            "http://value$with$non-permitted$ special$chars", // path with special non permitted chars
            "http:://www.goal.com",    //Using ':' twice
            "http://www/goal?¿que?",  //Using non-ASCII characters '?¿'
            "http:user@/goal",  //Missing '//' after scheme
            "http:user://*",               //Using '*' in authority
            "file:///c:/|documents",       //Using '|', which is not allowed in path
            "http:wrong:wrong?",              //"wrong" shouldn't appear after scheme
            "http://@@@@@@@",           //Using '@' multiple times in userinfo/password
            "http://%zz",              //Invalid percent encoding"http:/wrong//",              //Using single '/' after scheme, '//'
            "http://localhost===",       //Using '===' in host which is illegal
            "http:::::://",       // Contains multiple colons after the scheme.
            ":http://example.com", // Character before the scheme.
            "http://example.com:", // Unnecessary colon at the end.
            "@http://example.com", // Character "@" before the scheme.
            "http:///example.com", // Duplicate / after scheme
            ":/example/com@example.com", // Special characters in the wrong places.
            "http/://example.com", // Extra / in the scheme.
            "http://example..com", // Consecutive .. in host.
            "http://.example.com",  // Leading . in host.
            "#?%:",    // Accepted but it starts with '#'
            "a:b:c",   // Multiple ':' in uri
            "http.",   // Not allowed to end with '.'
            "//@user", // Userinfo not allowed with only double slash no scheme
            "       ", // Strings of space, it isn't null or has length 0 but surely isn't a URI
            ".",       // Single '.'
            "1http://example.com", // Scheme starting with number
            "ftp://www.example,com", // Comma used instead of dot in domain
            "http:hostname", // No "//" after scheme
            "A#bc?",    // Question mark '?' at in fragment
            "http://..",
            "http://@@",
            "10080://abc",
            "http:://abc",
            "http:///",
            "http;//example.com",
            "http://...",
            "http:///example",
            "./unclearRelative/../../reference",
            "http://276.0.0.0",
            "ftp://xyz@/",
            "http://;;;",
            "https://user;;;@localhost",
            "###",
            "http://usern#ame@localhost",
            "http://@@/",
            "http:///hello",
            "%6a%61%76%61%2e%63%6f%6d",
            "@@@",
            ":////",
            "http://valid_but_no_tld",
            "http:://[2001:db8::7]/c=GB",
            "http:// hostname",
            "http://user@hostname",
            "@",
            "ftp://invalid@username:password.com/",
            "http:///a",
            "magnet:?@",
            "http:three_slashes_path",
            "http://user:password@foo.com/#extra@sign",
            "http://google.com:",
            "file.-#misplacedhash",
            "Ischeme:/meaning:lesscolon",
            "http:///some-percent/%S/som%apth-here",
            "http://:",
            "http///",
            "aaaaaa:ppppppssss",
            "kkkk.ssss::",
            "https//wrong/semicolon.org",
            "https-//minusdoubleslash.io",
            "a@#.com",
            "aDigiIncorrect$24@tftma.com",
            "abcdef::::..",
            "http:+@/",
            "http://valid@tilnow_with.valid?stuff#in_it?-+_,;~::.!$'()",
            "!valid.com",
            "&valid.com",
            "~/valid/pch.array.com?question=%remove241%",
            "http:::/valid.no:doubt",
            "*.do.tdot.dollar::%%twenty##fiver%",
            "H://cannot.must,/fail::.fundamentally.broken",
            "full@@::Fate?then5ev.er3##now2next",
            "yes:://server.com?$fewquerythem?some$^script=in//jection[]",
            "no::true.isherename.now!~mill",
            "__services.no.se.all..me",
            "http:::.allowed/port80.com",
            "[%ipv6]::hexdigit-fail[]",
            "smashing::mel.the.web?~~-~-?-gem?",
            "5times::.charco.@00-2_mor?find%answer#[]",
            "//::authority/path",
            "http://example.com",
            "1_scheme://example.com",
            "http://example.com:8080",
            "http://example.com:65536",
            "http://example.com ?query",
            "http://example.com#fragment#invalid",
            "http://_domain.com#fragment",
            "mailto:name@example.com",
            "ftp://user:pass@example.org",
            "http:://example.com",
            "/relative_path",
            "//authority.com/",
            ".invalid_scheme://example.com",
            "//[::1]:8080/ipv6/url",
            "http//not.a.valid.url",
            "http//missing.colon.example",
            "http://U413418b8ac4a2.example.org",
            "http://:[2001:db8::7]/c=GB?objectClass=one",
            "http://example.com/%7Eusername/",
            "http://example<>?%3Ecom/not_valid",
            "httpsABC//wrong_scheme.name",
            "file:///local/path/like/this",
            "IPv6://[fe80::c604:b34a]%25en1",
            "sms:+19174054262",
            "",  //empty string
            "://missing_scheme.com",
            "http:missing//slash_2/",
            "scheme1|invalid_d://non.url",
            "scheme_missing_here://pmcai.etxae5.auast",
            ":/_wrong/syntax///ftp/av.example",
            "/slash/starting/URI/not",
            "scheme_suffix://marks21.examples.",
            "urt://Scheme_N0T_G!ven.net",

            "80://denotes.invalid_greq.quest",
            "#valid_fragment",
            "/justpath",
            "path?q=query",
            "?naked_query",
            "h://example.äöü%@!$&'",
            "foo#any0one",
            "partition//#",
            "harvar####dsquare/",
            "unexpected!/..comes}",
            "./?dot14mov.ip%%ENCODED",
            "//whatever123/",
            "dancing//queen:ABBA",
            "space inbetween",
            "'latincharx:gh$$$///V",
            "localhost/",
            "http://www.alpha-beta.com",
            "thina;/!:0*&%%+`",
            "some://Labe'Lthat.allowed/",
            "www.basic.com/",
            "^addfunc:/Unexpected'",
            "/lima:///delta.com",
            "&g1-DAVE<alphanumb..",
            "1300:1://endzone13num",
            "253_533/&ha()-*$.:",
            ")!at.galid.com",
            "/silli&&?<*5)%-|",
            "#142-%\"#123}'\",`-9-/",
            "@n.u/chopintray_13NQ0",
            "file:/home/user/.config/.vimrc",
            ":/./?:/:/",
            ")\"%\"",
            "+/!\")?-<::@/]`",
            "//localhost:5432/database",
            "/unn:)-%alled_alpha246",
            "')&threo^)-|::8rr1268_/'",
            "IDNA_example:пример.тест19",
            "http:/errors.cloud77_EX/2e",
            "MAIN_STREAM%727}))9551%%",
            "/@??AD?>Kgehw.:123-dd_48()",
            "://such:?beginningstring.",
            "INVALID_*place|)006;}%#%%url",
            ">change>s!+:-5.isout404??",
            "@/absolute.appending-list<W9",
            "]|[IE_PI-code93)$D\":-`03UTI%",
            "(here/=)/in.particular90}",
            "Hier.main//EXAMPLE_'R'<:0@``",
            "}<D??#%%>+41#{>:%^)>WAYWAY",
            "-&$HAPPYHAPP0^>@:!)\"]-**RAYS",
            "http://example.com",
            "https://example.com",
            "ftp://example.com",
            "http://",
            "http://user@:password@example.com",
            "http://user@example.com:80",
            "http://example.com:80/",
            "http://example.com/path",
            "http://example.com/path/",
            "http://example.com/path//",
            "http://example.com/path/?query",
            "http://example.com/path/?",
            "http://example.com/path#fragment",
            "http://example.com/path#",
            "http://example.com/path/?query#fragment",
            "http://example.com/path?#",
            "http://example.com?query",
            "http://example.com#fragment",
            "http:///path",
            "http://:password@example.com",
            "http://example.com/path/../",
            "http://example.com/path/./",
            "http://example.com:65536",
            "http://example.com:0",
            "http://example.com:-80",
            "http://example.com:80a",
            "http://example.com:a",
            "http://example.com#fragment#",
            "http://example.com?query#",
            "http://example.com?#fragment",
            "http://example.com?query#fragment#",
            "http://example.com?query?query",
            "http://example.com#fragment?query",
            "http://example.com/path/..",
            "http://example.com/path/../path",
            "http://example.com/path//path",
            "http://example.com/path/./path",
            "http://example.com:80/path",
            "http://example.com:80//path",
            "http://example.com:80/?query",
            "http://example.com:80/#fragment",
            "http://example.com:80?query",
            "http://example.com:80#fragment",
            "http://example.com:80?query#fragment",
            "http://example.com:80#fragment?query",
            "http://example.com:80#fragment#",
            "http://example.com:80?query#",
            "http://example.com:80?#fragment",
            "http://example.com:80?query#fragment#",
            "http://example.com:80?query?query",
            "http://example.com:80#fragment?query",
            "http://example.com/%",
            "http://example.com/%A",
            "http://example.com/%G0",
            "http://example.com/%0G",
            "http://example.com/%2",
            "http://example.com/%20",
            "http://example.com/%20%20",
            "http://example.com/%20%20%20",
            "http://example.com/%%%",
            "http://example.com/%%%20",
            "http://example.com/%20%%%20",
            "http://example.com/%20%20%%%20",
            "http://example.com/%20%20%20%%%20",
            "http://example.com/%20%20%20%20",
            "http://example.com/%20%20%20%20%20",
            "http://example.com/%20%20%20%20%20%20",
            "http://example.com/%20%20%20%20%20%20%20",
            "http://example.com/%20%20%20%20%20%20%20%20",
            "http://example.com/%20%20%20%20%20%20%20%20%20",
            "http://example.com/%20%20%20%20%20%20%20%20%20%20",
            "http://example.com/%%",
            "http://%20",
            "http://%20%20",
            "http://%20%20%20",
            "http://%20%20%20%20",
            "http://%20%20%20%20%20",
            "http://%20%20%20%20%20%20",
            "http://%20%20%20%20%20%20%20",
            "http://%20%20%20%20%20%20%20%20",
            "http://%20%20%20%20%20%20%20%20%20",
            "http://%20%20%20%20%20%20%20%20%20%20",
            "http://%20%20%20%20%20%20%20%20%20%20%20",
            "http://%20%20%20%20%20%20%20%20%20%20%20%20",
            "http://%20%20%20%20%20%20%20%20%20%20%20%20%20",
            "http://%20%20%20%20%20%20%20%20%20%20%20%20%20%20",
            "http://%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20",
            "http://%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20",
            "http://%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20",
            "http://%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20",
            "http://%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20",
            "http://%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20",
            "http://%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20",
            "http://%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20",
            "http://%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20",
            "http://%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20",
            "urn:example:12345678901234567890123456789012345",
            "urn::",
            "urn:example:",
            "urn:ex-ample:123",
            "urn:1234567890123456789012345678901234567890:sample",
            "urn:example:sample?+",
            "urn:example:sample?=",
            "urn:example:sample#",
            "urn:example:sample?+?",
            "urn:example:sample?=!",
            "urn:example:sample?++",
            "urn:example:sample?=,",
            "urn:example:sample?+;",
            "urn:example:sample?=:",
            "urn:example:sample?+=",
            "urn:example:sample?+@",
            "urn:example:sample?=.",
            "urn:example:sample?+%",
            "urn:example:sample?=&",
            "urn:example:sample?+/",
            "urn:example:sample?+?",
            "urn:example:sample?+=#",
            "urn:example:sample?+%%",
            "urn:example:sample?+=%",
            "urn:example:sample?+;+",
            "urn:example:sample?+=:",
            "urn:example:sample?+%;",
            "urn:example:sample?+=!",
            "urn:example:sample?+!;",
            "urn:example:sample?+=,",
            "urn:example:sample?+;:",
            "urn:example:sample?+=-",
            "urn:example:sample?+?:",
            "urn:example:sample?+=.",
            "urn:example:sample?+!:",
            "urn:example:sample?+=@",
            "urn:example:sample?+!;",
            "urn:example:sample?+=",
            "urn:example:sample#;",
            "urn:example:sample#=,",
            "urn:example:sample#:.",
            "urn:example:sample#:",
            "urn:example:sample#/",
            "urn:example:sample#;",
            "urn:example:sample#=",
            "urn:example:sample#!",
            "urn:example:sample#?",
            "urn:example:sample#%%",
            "urn:example:sample#*",
            "urn:example:sample#-",
            "urn:example:sample#.",
            "urn:example:sample#:",
            "urn:example:sample#@",
            "urn:example:sample#_",
            "urn:example:sample#!;",
            "urn:example:sample#&",
            "urn:example:sample#'",
            "urn:example:sample#(",
            "urn:example:sample#)",
            "urn:example:sample#*",
            "urn:example:sample#+",
            "urn:example:sample#,",
            "urn:example:sample#;",
            "urn:example:sample#=",
            "urn:example:sample#?",
            "urn:example:sample#@!",
            "urn:example:sample#@:",
            "urn:example:sample#@;",
            "urn:example:sample#@,",
            "urn:example:sample#@=",
            "urn:example:sample#@?",
            "urn:example:sample#@/",
            "urn:example:sample#@'",
            "urn:example:sample#@:",
            "urn:example:sample#@(",
            "urn:example:sample#@)",
            "urn:example:sample#@*",
            "urn:example:sample#@+",
            "urn:example:sample#@$",
            "urn:example:sample#@%",
            "urn:example:sample#@&",
            "urn:example:sample#@'",
            "urn:example:sample#@(",
            "urn:example:sample#@)",
            "urn:example:sample#@*",
            "urn:example:sample#@+",
            "urn:1234567890123456789012345678901:test", // NID is 31 characters long
            "urn:example:1.2.3", // NSS contains periods
            "urn:example:this%GHIisinvalid", // Invalid pct-encoded
            "urn:example:test?+rcomponent?=qcomponent#fcomponent", // Missing spaces between r-component and q-component
            "urn:example:test?+rcomponent?=qcomponent?#fcomponent", // Extra "?" in q-component
            "urn:example:test#fcomponent?+rcomponent?=qcomponent", // r-component and q-component after f-component
            "urn:example:test?+rcomponent#fcomponent?=qcomponent", // q-component after f-component
            "urn:example:test?=qcomponent#fcomponent?+rcomponent", // r-component after f-component
            "urn:example:test?+rcomponent!#fcomponent", // Invalid character "!" in r-component
            "urn:example:test?=qcomponent$#fcomponent", // Invalid character "$" in q-component
            "urn:example:test#fcomponent?+rcomponent", // r-component inside f-component
            "urn:example:test#fcomponent?=qcomponent", // q-component inside f-component
            "urn:example:test?+rcomponent#fcomponent&", // Invalid character "&" in f-component
            "urn:example:test?=qcomponent#fcomponent(", // Invalid character "(" in f- component
            "urn:example:test#fcomponent;?+rcomponent", // Invalid character ";" in f-component
            "urn:example:test#fcomponent+?=qcomponent", // Invalid character "+" in f-component
            "urn:example:test?+rcomponent,?=qcomponent", // Invalid character "," in r-component
            "urn:example:test?+rcomponent#fcomponent=", // Invalid character "=" in f-component
            "urn:example:test#fcomponent?+rcomponent@?=qcomponent", // Invalid character "@" in f-component
            "urn:example:test?+rcomponent#fcomponent*?=qcomponent",  // Invalid character "*" in f-component
            "urn:example:123#", // invalid because fragment component is empty
            "urn:example:123?+", // invalid because r-component is empty
            "urn:example:123?=", // invalid because q-component is empty
            "urn:example:123?+=?", // invalid because both r-component and q-component are empty
            "urn:example:123?+?=foo", // invalid because both r-component and q-component should not be present simultaneously
            "urn:example:123?+=foo?=bar", // invalid because both r-component and q-component should not be present simultaneously
            "urn:example:123?+foo?=bar", // invalid because both r-component and q-component should not be present simultaneously
            "urn:example:123?+foo?+bar", // invalid because multiple r-components are not allowed
            "urn:example:123?=foo?=bar", // invalid because multiple q-components are not allowed
            "urn:example:123?foo?=bar", // invalid because r-component should start with '?+'
            "urn:example:123?+foo?bar", // invalid because q-component should start with '?='
            "urn:example:123?foo?bar", // invalid because neither r-component nor q-component have the correct starting characters
            "urn:example123:foo", // invalid because NID is missing the ':' delimiter
            "urn::example123:foo", // invalid because NID is missing
            "urn:example123::foo", // invalid because NID delimiter ':' is misplaced
            "urn:example:123?+foo?=", // invalid because q-component is empty after '?='
            "urn:example:123?+=foo?+bar", // invalid because both r-component and q-component should not be present simultaneously
            "urn:example:123?=foo?+bar", // invalid because both r-component and q-component should not be present simultaneously
            "urn:example:123?+foo?+=bar", // invalid because both r-component and q-component should not be present simultaneously
            "urn:example:123?+=foo?+bar?=", // invalid because both r-component and q-component should not be present simultaneously and q-component is empty
            "urn:example:123#", // invalid because fragment component is empty
            "urn:example:123?+", // invalid because r-component is empty
            "urn:example:123?=", // invalid because q-component is empty
            "urn:example:123?+=?", // invalid because both r-component and q-component are empty
            "urn:example:123?+a=1?=b", // invalid because r-component contains an '=' character
            "urn:example:123?=a+1?+b", // invalid because q-component contains a '+' character
            "urn:example:123?+a?+b", // invalid because multiple r-components are not allowed
            "urn:example:123?=a?=b", // invalid because multiple q-components are not allowed
            "urn:example:123?+a?=b", // invalid because both r-component and q-component should not be present simultaneously
            "urn:example:123?+a?b", // invalid because q-component should start with '?='
            "urn:example:123?foo?bar", // invalid because neither r-component nor q-component have the correct starting characters
            "urn:example123:foo", // invalid because NID is missing the ':' delimiter
            "urn::example123:foo", // invalid because NID is missing
            "urn:example123::foo", // invalid because NID delimiter ':' is misplaced
            "urn:example:123?+foo?=", // invalid because q-component is empty after '?='
            "urn:example:123?+=foo?+bar", // invalid because both r-component and q-component should not be present simultaneously
            "urn:example:123?=foo?+bar", // invalid because both r-component and q-component should not be present simultaneously
            "urn:example:123?+foo?+=bar", // invalid because both r-component and q-component should not be present simultaneously
            "urn:example:123?+=foo?+bar?=", // invalid because both r-component and q-component should not be present simultaneously and q-component is empty
            "urn:example:31-!@#%^&*()_+[];',./{}|:\"<>?~`", // invalid because NID contains invalid characters
            "http://example.com/ünicode", // invalid because non-ASCII characters must be percent-encoded
            "mailto:user@exampl e.com", // invalid because whitespace is not allowed in domain names
            "http:///path", // invalid because there is an empty authority (i.e., host is missing)
            "http://example.com:12a34", // invalid because the port contains a non-digit character
            "http://example.com#frag#ment", // invalid because the fragment contains an extra "#"
            "http://example.com/path?query=val&key=valué", // invalid because non-ASCII characters must be percent-encoded in query
            "http://user@:password@example.com", // invalid because the userinfo contains a colon without a password
            "http://example.com:65536/path", // invalid because the port number is out of range
            "http://example..com", // invalid because there are two consecutive dots in the domain name
            "http://example_com", // invalid because underscore is not allowed in domain names
            "ftp:://example.com", // invalid because there is an extra colon in the scheme
            "http://[2001:0db8:85a3:0000:0000:8a2e:0370:7334", // invalid because the IPv6 address is not enclosed by square brackets
            "http://example.com/%C3%A4%F0%9F%98%81", // invalid because the percent-encoded sequence contains a surrogate pair
            "http://example.com/path/../other", // invalid because the path contains a dot-segment that should be removed
            "http://example.com/?query&&key=value", // invalid because the query contains two consecutive ampersands
            "http://example.com#frag?ment", // invalid because the fragment contains a reserved character "?"
            "http://example.com/path#frag?ment", // invalid because the fragment is not separated from the path by a "?"
            "http://example.com/path#%C3%A4", // invalid because the fragment contains a percent-encoded non-ASCII character
            "http://example.com/%E0000", // invalid because the percent-encoded sequence contains a private use character
            "http://example.com/path#%D800", // invalid because the percent-encoded sequence contains a surrogate character
            "http://.example.com/", // invalid because the domain starts with a dot
            "http://example.com..", // invalid because the domain ends with two dots
            " Http://example.com", // invalid because there's a leading whitespace before the scheme
            "Gey://example.com/", // invalid because the scheme contains a non-alphanumeric character
            "http://2001:0db8:85a3:0000:0000:8a2e:0370:7334", // invalid because the IPv6 address is not enclosed in square brackets
            "mailto:user@@example.com", // invalid because there are two consecutive at symbols in email
            "https://user%yoiga@example.com/path", // invalid because the percent-encoded character in userinfo is not read
            "http://:p@ssword@example.com", // invalid because the userinfo section doesn't have a user but password is present
            "http://example.com/p[]ath", // invalid because the square brackets are reserved characters in the path
            "://example.com/path", // invalid because the scheme is missing
            "-ftp://example.com", // invalid because there's an unexpected character before the scheme
            "+smtp://example.com", // invalid because the scheme starts with an unexpected character
            "http://example.-com", // invalid because domain part starts with "-"
            "ttp://example.com", // invalid because it contains a control character in the scheme
            "http:// example.com", // invalid because there's a space between the scheme and the authority section
            "http:///999", // invalid because there's an empty authority section (i.e., host is missing)
            "ftp:user:pass@example.com", // invalid because a double slash is expected after a scheme
            "http%3A//example.com", // invalid because the scheme contains percent-encoded characters
            "http://www.example.256.256.", // invalid because an incorrectly escaped IPv4 address is treated as a part of the domain
            "http://ab测试.com/", // invalid due to the mix of letters mixed script in the domain
            "http://✪linkedin.profile✪.example.com", // valid; Unicode characters are allowed in ireg-name
            "https://mysite.%C2%A9test", // valid; pct-encoded reserved characters are allowed in ireg-name
            "ftp://example.com/path/to%2Finfo",  // valid;, use of path delimiter reservation escaped representation in path ("/" => "%2F")
            "http://example.com/%3B+%2A%27",  // valid; URL-encoded sub-delims normally reserved (;+*' => %3B+%2A%27)
            "http://[faby::DEA1:03::effe]",  // valid; short IPv6 address with mixed capitalizations
            "mailto:user@test@example.com", // valid; because it is treated as iquery part (which accepts really valid email address -> user@test.example.com)
            "ftp://user:pa%23sword@example.com", // valid; password contains '#' encoded
            "http://example[.]com", // valid; escaped sequence is treated as ireg-name since "%x5D" ("]") occurs first
            "http://com.test", // valid; no first-level domain / reversed domain names
            "ą://example.com:ż", // valid; Unicode upper code (< %xA0-D7FF>) scheme and host port
            "tel:测试@example.com", // valid within non Anglophone character-only url name in International (& valid e.164 number convention incompatible for nondigit)
            "httpᝣ://example𛆷.com?quiery=s%C2%BBr%c2%b4%C2%AUX_VALUE", // valid; Unicode even longer code > %xE63DF7FF & relevant internal framework expressed forms used
            "urn:name::test-place&part.data>456.data989/value00", // valid; colon sub delimiter positions to rearrange (QSA exclusion) representation
            "http://example.com/%C2%A0", // valid but looks invalid due to percent-encoded non-breaking space character
            "http://example.com/%E0%A4%A4", // valid but looks invalid due to percent-encoded multibyte Unicode character
            "http://example.com/%23", // valid but looks invalid due to percent-encoded '#' character
            "http://example.com/..", // valid but looks invalid due to two consecutive dots
            "http://example.com/./", // valid but looks invalid due to dot and slash combination
            "http://example.com/:@/", // valid but looks invalid due to colon and '@' combination in path
            "http://example.com/%20%20", // valid but looks invalid due to percent-encoded spaces
            "http://example.com/()/", // valid but looks invalid due to parentheses in path
            "http://example.com/%7B%7D/", // valid but looks invalid due to percent-encoded curly braces
            "http://example.com/!$&'()*+,;=", // valid but looks invalid due to sub-delims characters in path
            "http://example.com/%2F%3A%40", // valid but looks invalid due to percent-encoded gen-delims characters
            "http://example.com/;", // valid but looks invalid due to a semicolon at the end of the path
            "http://example.com/?%E000", // valid but looks invalid due to percent-encoded private character in query
            "http://example.com/%3D", // valid but looks invalid due to percent-encoded '=' character
            "http://example.com/%2C", // valid but looks invalid due to percent-encoded ',' character
            "http://example.com/%40", // valid but looks invalid due to percent-encoded '@' character
            "http://example.com/[]", // valid but looks invalid due to square brackets in path
            "http://example.com/%5D%5B", // valid but looks invalid due to percent-encoded square brackets
            "http://example.com/%C3%80%C3%BF", // valid but looks invalid due to percent-encoded multibyte Unicode characters
            "http://example.com/%F0%9F%98%8A", // valid but looks invalid due to percent-encoded emoji character
            "http://[1080::8:800:200C:417A]/foo", // valid because it is a valid IPv6address. Looks invalid because of the hexadecimal value in the domain part.
            "example.com:#fragment", // valid because fragment (after '#') can be empty. Looks invalid because it mimics an unnamed HTML anchor.
            "ftp://ftp.is.co.za/rfc/rfc1808.txt", // valid  because it is a simple text file on a FTP server. Looks invalid as many people may not even know HTTP or HTTPS protocols.
            "#", // valid since it is a reference to the document itself. Looks invalid as there are no scheme, host, or path specified.
            "mailto:", // valid because scheme doesn't require further components. Looks invalid as no email address specified.
            "http://192.0.2.235:80/", // valid  because it specifies the port explicitly, which is optional. Also, IP addresses are acceptable for hosts.
            "/relative/uri/with/absolute/path/to/resource.txt", // valid since relative URIs can start with '/'. Looks invalid since there's no scheme and host.
            "http://example.com?#", // Valid although it has an empty query and fragment parts following '?' and '#'. May look invalid due to seemingly misplaced '?#' characters.
            "//foo.com/bar?p=test#", // protocol (scheme) relative URL which is valid but looks wrong due to absence of http or https etc before the slashes
            "file:///home/user", // refers to a local file which makes it look different from most web URLs we encounter everyday
            "g:h", // minimal absolute URI with short scheme and path, making it look incomplete
            "\u4E2D\u56FD\u7F51@www.website.cn", // Using Unicode in authority, makes it look different and seemingly invalid because users are more familiar with ASCII characters in URL.
            "//::1/foo/bar?q=123#abc", // Uses an IPv6 loopback (localhost) address, something that a regular user doesn't usually come across everyday
            "//localhost:8080/foo/bar?q=123#abc", // uses port number 8080 which is typically associated with locally served web servers during development phase. May seem off to regular users who mostly encounter URLS without port numbers indicated i.e., using default ports--80 for HTTP and 443 for HTTPS
            "http://user:pass@www.website.com/path/file.aspx?qs=param#fragid1_fragid2?sortedBy=values&sortBy=desc", // uses multiple sub-fragments making use of '_' separator and fragments having own parameters--uncommon but very much possible
            "", // empty string represents current document URL--valid yet unusual
            "%E4%BD%A0%E5%A5%BD@www.website.cn" ,// applications may percent encode special characters i.e., non-ASCII characters
            "?emptyRelUrl#frag", // Relative URL starting with query component; although not often seen in browsers' address bar
            "/" ,// Relative URL with root path - it's simply a shorthand for root directory
            "javascript:function(){alert('Hello')};", // A javascript URL where the contents after : constitute javascript code
            "https://example.com/irifile#frag ment", // invalid because fragment contains space
            "https:example.com/irifile", // invalid because there is no "//" after the colon in the scheme
            "http://ex..com", // invalid because host part contains two consecutive periods
            "https//example.com", // invalid because there is missing colon after scheme
            "http://255.256.255.255", // invalid because IPv4 address octet exceeds 255
            "ftp://usern@me:password@example.com", // invalid because userinfo contains '@' character
            "http://usern ame:password@example.com", // invalid because userinfo contains whitespace
            "http:@example.com/file", // invalid because there is ':' in the iuserinfo section
            "http://@example.com/file", // invalid because iuserinfo exists but it's empty
            "http//[example.com]", // invalid because '//' replaced with characters '[]'
            "http:/example.com/path", // invalid because single slash '/' after scheme colon ':'
            "//www.example.com:buyproduct?", // invalid because missing scheme
            "http:/www.example.com:3u0932", // invalid because non-digit character in port
            "http::/--localhost/", // invalid as it got double colons "::"
            "[::1].com", // invalid as IPv6 address should not be combined with .com
            "/path?query=value#fragment#",  //invalid as it's a relative but starts with '/' ,
            "?myvar=value&var2=anothervalue.example.net/path.html",  //invalid as query section comes before URL,
            "#test.example.in/user/test.cgi?id=1345-pnt456#",  //invalid as fragment comes before URL,
            "/folder/file.htm_",  //invalid as underscore comes in the resource name,
            "www.example./index.php",  //invalid due to trailing dot.
            "http:www.example.com", // invalid because it lacks '//' after the colon in the 'scheme'
            "https:// user@example.com", // invalid because of the space between the protocol and authority
            "https://www.example.com/#foo?bar", // invalid because query string should come before fragment identifier
            "http:://www.example.com", // invalid because it uses two colons after 'scheme'
            "http:/www.example.com/", // invalid because it uses only a single '/' after 'scheme'
            "http://𠜎" ,  // invalid because non-ASCII unicode characters are present in domain name
            "http://example.com:port", // invalid because 'port' is not numeric
            "@example.com", // invalid because it lacks scheme and domain is prefixed with '@'
            "http://www.example.com/ path", // invalid because of the space in the path
            "https:\\\\example.com", // invalid because uses two backslashes instead of forward slashes
            "HTTP//www.example.com", // invalid because it lacks colon after scheme
            "http://///www.example.com/", // invalid because it uses more than two slashes after scheme
            "", // an empty string, so definitely an invalid IRI
            "mailto:john.deo@example .com",  // invalid since there is a space in authority part
            ":example.com/",  // missing scheme in the IRI
            "/example",  // missing scheme and authority in the IRI.
            "&*(&^%%^^&&@$#~~`|?>>>//////\"",  // using special characters which are not allowed.
            "http://uc::8001/index.html",  //double colon usage.
            "//192.0.2.16:80/",  // missing Scheme.
            "***@***.com",  // wildcard character usage in email formation.
            "http:www.example.com", // invalid because it lacks "//" after the colon (scheme and path confusion)
            "http://www.example.com/path#frag?query", // invalid because query string should come before fragment
            "http://user@www.example.com:80path", // invalid because no '/' between port and path
            "https:/www.example.com", // invalid because uses only a single '/' after 'scheme'
            "ftp://user@www.example.net:port", // port must be a number but it's a non-numeric string here
            "http://[2001:0db8:85a3:0000:0000:8a2e:0370]1",  // invalid as IPv6 address is illegal (no closing bracket)
            "https://example.com:^80/", // port section of authority contains non-numeric characters
            "https://example.com\ud800",  // invalid because it includes unpaired surrogate \ud800
            "https://www.example.com/ path#frag", // Invalid due to space in path
            "https:xn--example.com",   // invalid punycode in host, "--" is not allowed at the start of a label
            "+https://www.example.com/",   // invalid scheme, protocols don't start with "+"
            "mailto:john@exampl\\xe9.com",  // Invalid email format, uses non-ASCII character
            "http://%41%6D%65%72%69%63%61//index.html",  // Percentencoded octets in host part
            "%20//google.com",  // missing Scheme.
            "http://%7Flocalhost/",   // Hostname contains unallowed ASCII %7F
            "-http://example.com/",   // The scheme must begin with a letter.
            ";http://google.co.uk/",  // unallowed semicolon at start.
            "file:///c:/WINDOWS/clock.avi //</windows/file/urls/arent/iri>",  // comment in Windows file URL
            "\u026Ftp://localhost/test?key=value",   // Unicode character at the start of scheme part
            "http://www.example.com:80port", // invalid because 'port' is not separate from numeric port value
            "http://www.example.com:", // invalid because no port number after colon
            "https:/example.com", // invalid because it lacks a '/' after 'scheme'
            "http://www.e.com#frag#ment", // invalid due to extra '#' in fragment
            "HTTP://www.example.com", // invalid because scheme must be lowercase
            "http://123.123.123.123:8080..", // invalid due to trailing dots
            "https://www.e:8080x/path/index.html", // port contains non-numeric character
            "http://site.", // invalid because host ends with a period
            "http:///www.example.com", // invalid since there's an extra '/'
            "αtp://www.example.com", // scheme contains non-latin character
            "https://@example.com", // userinfo is present but empty
            "https:/#www.example.com", // host name starts with '#'
            "http://%67oogle.com/",   //hostname starts with percent-encoded value which should not be in case of hostname.
            "ftp:/user@example/serverpath",  // missing one '/' in front of authority section.
            "mailto:user@.com",  // email address's domain starts with '.'
            "http://www.example.com::80", // invalid because there shouldn't be two colons before port
            "https://:?query=example", // invalid because hostname is just a punctuation mark
            "ftp://user:password@host:path/path", // invalid because 'path' should start with '/'
            "gophers:/marcin.com/", // invalid due to missing '/' after scheme in URI structure.
            ".www.example.com", //Invalid as it starts with '.' which is not/unable to parse
            "http:://www..example..com/", // Invalid as there are more than one dots between www and example and between example and com. Also has double colon after http protocol.
            ":_@tcp(10.0.0.1:3306)/dbname?param=value#fragmentId",  //No scheme but contains userinfo, hostinfo, query & fragment.
            "_:@./dbname?param=value#fragmentId"                      ,// Missing scheme and host part altogether just userinfo presented which is '_' .
            "_:@tcp(10/0/0/1:3306)./dbname?param=value#fragmentId"   ,//Forward slash used instead of dot in IP Address.',
            "_:@tcp(10001:3306)./dbname?param=value#fragmentId"      ,//IP address > maximum possible value (255) in initial octet i.e., 10001 instead of within range (<=255)
            "_:!$&\\'()*+,-.=^`|~.@192.*.*.*./dbnameSpace?param=value #frag mentI d"   ,//It contains multiple symbols from sub-delims group against rfc3987
            "!##''\"\""                                               ,////symbols only present no specific parts defined or if symbols exist they not according RFC-3987 rules
            "[uri-scheme]:authority:/relative-path/query/data#"       ,
            "%20%21%22%23%24 %25_%A_B%C_D:E;F <G>H/I=J-K[L]M@N.OP,Q(R)S*T+U,V=W:X>Y-Z[\\]^`,a{b}|c¿©¬º¡¶§μ£¥¨\u200C\u200B•\u00AD-[!]–—«»\"<)>#$*\n." ,
            "\"[#]\\†‰‹›‡·‚฿¤₳＄₵¢₡₢$₫₯֏ް€ƒ₣𐆚£៛﷼ლℳ元圓円﹪％؋〒☹ن╭∩╮،งจขชไำ้ /_/¯/(?_?)\\?(._.)<( “_” )>@(^_^@$*(^_^)*//(’-’)-CCC-combo breaker!!/",
            "&T���ڀ����ӂ�_-���� +-?,+#═╗║╚╝═ \"\\'",
            "#!wiki=(Category)", "/projects/{project}/repos/{repository}/pull-requests/{pullRequestId}/overview",
            "{scheme}:{scheme-specific-part}[#{fragment}]",
    };

        String[] uri = {
                "http://",
                "http://~/",
                "http://-/",
                "http://./",
                "http://_/",
                "http://abc.def.k/",
                "http://abc...def.k./",
                "http://abc...def.k.",
                "http://.abc...def.k.",
                "http://exampl.org//",
                "http://exampl.org/",
                "http://exampl.org/_/",
                "http://exampl.org/a//b",
                "http://exampl.org/a/../b",
                "http://exampl.org/a?",
                "http://exampl.org/a?#",
                "http://exampl.org/a#",
                "http://exampl.org/a?a",
                "http://exampl.org/a%2?a=b",
        };

    @Test
    public void testCase() {

        for (String s : uri) {
            if (PartiallyImplementedUriValidator.isDefinitivelyValidAbsoluteUri(s)) {
                if (!isActuallyAbsolute(s)) {
                    System.out.println(s);
                }
            }
        }

        for (String s : uri) {
            if (PartiallyImplementedUriValidator.isDefinitivelyValidAbsoluteUri(s)) {
                assertTrue(isActuallyAbsolute(s));
            }
        }


    }

    @Test
    public void testCaseExtended() {

        for (String s : uriExtended) {
            if (PartiallyImplementedUriValidator.isDefinitivelyValidAbsoluteUri(s)) {
                if (!isActuallyAbsolute(s)) {
                    System.out.println(s);
                    isActuallyAbsolute(s);
                    PartiallyImplementedUriValidator.isDefinitivelyValidAbsoluteUri(s);
                }
            }
        }

        for (String s : uriExtended) {
            if (PartiallyImplementedUriValidator.isDefinitivelyValidAbsoluteUri(s)) {
                assertTrue(isActuallyAbsolute(s));
            }
        }


    }

    @Test
    public void testCaseSimple() {

        String s = "http://example.org/abc/def?ghi=jkl#iriog8349";
            if (PartiallyImplementedUriValidator.isDefinitivelyValidAbsoluteUri(s)) {
                if (!isActuallyAbsolute(s)) {
                    assertTrue(isActuallyAbsolute(s));
                }
            }


    }

    private boolean isActuallyAbsolute(String s) {
        try {
            boolean absolute = new URI(s).isAbsolute();
            return absolute;
        } catch (java.net.URISyntaxException e) {
            return false;
        }
    }

}
