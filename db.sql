--
-- PostgreSQL database dump
--

-- Dumped from database version 9.6.19
-- Dumped by pg_dump version 9.6.19

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner:
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner:
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: oauth; Type: TABLE; Schema: public; Owner: digikey
--

CREATE TABLE public.oauth (
    id bigint NOT NULL,
    expires_at timestamp without time zone,
    refresh_token_expires_at timestamp without time zone,
    access_token character varying(64),
    refresh_token character varying(64)
);


ALTER TABLE public.oauth OWNER TO digikey;

--
-- Name: oauth_id_seq; Type: SEQUENCE; Schema: public; Owner: digikey
--

CREATE SEQUENCE public.oauth_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.oauth_id_seq OWNER TO digikey;

--
-- Name: oauth_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: digikey
--

ALTER SEQUENCE public.oauth_id_seq OWNED BY public.oauth.id;


--
-- Name: parts; Type: TABLE; Schema: public; Owner: digikey
--

CREATE TABLE public.parts (
    id bigint NOT NULL,
    digikey_pn character varying(32)
);


ALTER TABLE public.parts OWNER TO digikey;

--
-- Name: parts_id_seq; Type: SEQUENCE; Schema: public; Owner: digikey
--

CREATE SEQUENCE public.parts_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.parts_id_seq OWNER TO digikey;

--
-- Name: parts_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: digikey
--

ALTER SEQUENCE public.parts_id_seq OWNED BY public.parts.id;


--
-- Name: price_breaks; Type: TABLE; Schema: public; Owner: digikey
--

CREATE TABLE public.price_breaks (
    part_id bigint,
    quantity integer,
    unit_price double precision
);


ALTER TABLE public.price_breaks OWNER TO digikey;

--
-- Name: oauth id; Type: DEFAULT; Schema: public; Owner: digikey
--

ALTER TABLE ONLY public.oauth ALTER COLUMN id SET DEFAULT nextval('public.oauth_id_seq'::regclass);


--
-- Name: parts id; Type: DEFAULT; Schema: public; Owner: digikey
--

ALTER TABLE ONLY public.parts ALTER COLUMN id SET DEFAULT nextval('public.parts_id_seq'::regclass);

--
-- Name: oauth_id_seq; Type: SEQUENCE SET; Schema: public; Owner: digikey
--

SELECT pg_catalog.setval('public.oauth_id_seq', 2, true);

--
-- Name: parts_id_seq; Type: SEQUENCE SET; Schema: public; Owner: digikey
--

SELECT pg_catalog.setval('public.parts_id_seq', 93, true);


--
-- Name: oauth oauth_pkey; Type: CONSTRAINT; Schema: public; Owner: digikey
--

ALTER TABLE ONLY public.oauth
    ADD CONSTRAINT oauth_pkey PRIMARY KEY (id);


--
-- Name: parts parts_digikey_pn_key; Type: CONSTRAINT; Schema: public; Owner: digikey
--

ALTER TABLE ONLY public.parts
    ADD CONSTRAINT parts_digikey_pn_key UNIQUE (digikey_pn);


--
-- Name: parts parts_pkey; Type: CONSTRAINT; Schema: public; Owner: digikey
--

ALTER TABLE ONLY public.parts
    ADD CONSTRAINT parts_pkey PRIMARY KEY (id);


--
-- Name: price_breaks price_breaks_part_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: digikey
--

ALTER TABLE ONLY public.price_breaks
    ADD CONSTRAINT price_breaks_part_id_fkey FOREIGN KEY (part_id) REFERENCES public.parts(id);


--
-- PostgreSQL database dump complete
--

