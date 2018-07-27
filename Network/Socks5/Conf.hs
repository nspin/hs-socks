-- |
-- Module      : Network.Socks5.Conf
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- typical SOCKS configuration
module Network.Socks5.Conf
    ( SocksConf(..)
    , socksHost
    , socksPort
    , defaultSocksConf 
    , defaultSocksConfFromSockAddr
    , defaultSocksConfWithUsernamePassword
    , defaultSocksConfWithUsernamePasswordFromSockAddr
    ) where

import Network.Socket
import Network.Socks5.Types (SocksAddress(..), SocksHostAddress(..), SocksVersion(..), SocksUsernamePassword(..))
import qualified Data.ByteString.Char8 as BC

-- | SOCKS configuration structure.
-- this structure will be extended in future to support authentification.
-- use defaultSocksConf to create new record.
data SocksConf = SocksConf
    { socksServer  :: SocksAddress -- ^ SOCKS Address
    , socksVersion :: SocksVersion -- ^ SOCKS version to use
    , socksUsernamePassword :: Maybe SocksUsernamePassword -- ^ SOCKS username and password
    } deriving (Show,Eq)

-- | SOCKS Host
socksHost :: SocksConf -> SocksHostAddress
socksHost conf = ha where (SocksAddress ha _) = socksServer conf

-- | SOCKS Port
socksPort :: SocksConf -> PortNumber
socksPort conf = port where (SocksAddress _ port) = socksServer conf

-- | defaultSocksConf create a new record, making sure
-- API remains compatible when the record is extended.
defaultSocksConf = defaultSocksConf' Nothing

defaultSocksConfWithUsernamePassword = defaultSocksConf' . Just

defaultSocksConf' :: Maybe SocksUsernamePassword -> String -> PortNumber -> SocksConf
defaultSocksConf' creds host port = SocksConf server SocksVer5 creds
    where server = SocksAddress haddr port
          haddr  = SocksAddrDomainName $ BC.pack host

-- | same as defaultSocksConf except the server address is determined from a 'SockAddr'
--
-- A unix SockAddr will raises an error. Only Inet and Inet6 types supported
defaultSocksConfFromSockAddr = defaultSocksConfFromSockAddr' Nothing

defaultSocksConfWithUsernamePasswordFromSockAddr = defaultSocksConfFromSockAddr' . Just

defaultSocksConfFromSockAddr' :: Maybe SocksUsernamePassword -> SockAddr -> SocksConf
defaultSocksConfFromSockAddr' creds sockaddr = SocksConf server SocksVer5 creds
    where server       = SocksAddress haddr port
          (haddr,port) = case sockaddr of
                             SockAddrInet p h      -> (SocksAddrIPV4 h, p)
                             SockAddrInet6 p _ h _ -> (SocksAddrIPV6 h, p)
                             _                     -> error "unsupported unix sockaddr type"
