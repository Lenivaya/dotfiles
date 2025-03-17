--- @since 25.2.7

local M = {}

function M:peek(job)
	local start, cache = os.clock(), ya.file_cache(job)
	if not cache then
		return
	end

	local ok, err = self:preload(job)
	if not ok or err then
		return
	end

	ya.sleep(math.max(0, PREVIEW.image_delay / 1000 + start - os.clock()))
	ya.image_show(cache, job.area)
	ya.preview_widgets(job, {})
end

function M:seek(job)
	local h = cx.active.current.hovered
	if h and h.url == job.file.url then
		local step = ya.clamp(-1, job.units, 1)
		ya.manager_emit("peek", { math.max(0, cx.active.preview.skip + step), only_if = job.file.url })
	end
end

function M:doc2pdf(job)
	local tmp = "/tmp/yazi-" .. ya.uid() .. "/" .. ya.hash("office.yazi") .. "/"

--[[	For Future Reference: Regarding `libreoffice` as preconverter
	  1. It prints errors to stdout (always, doesn't matter if it succeeded or it failed)
	  2. Always writes the converted files to the filesystem (so no Mario|Bros|Piping|Magic|To>stdout) --]]
	local libreoffice = Command("libreoffice")
		:args({
			"--headless",
			"--convert-to",
			"pdf:draw_pdf_Export:{" ..
				"\"PageRange\":{" ..
					"\"type\":\"string\"," ..
					"\"value\":" .. "\"" .. job.skip + 1 .. "\"" ..
				"}" ..
			"}",
			"--outdir",
			tmp,
			tostring(job.file.url)
		})
		:stdin(Command.NULL)
		:stdout(Command.PIPED)
		:stderr(Command.NULL)
		:output()
		
	if not libreoffice.status.success then
		ya.err(libreoffice.stdout:match("LibreOffice .+"):gsub("%\n.*", "") .. " " .. libreoffice.stdout:match("Error .+"):gsub("%\n.*", ""))
		return nil, Err("Failed to preconvert `%s` to a temporary PDF", job.file.name)
	end

	local tmp = tmp .. job.file.name:gsub("%.[^%.]+$", ".pdf")
	local read_permission = io.open(tmp, "r")
	if not read_permission then
		return nil, Err("Failed to read `%s`: make sure file exists and have read access", tmp)
	end
	read_permission:close()

	return tmp
end

function M:preload(job)
	local cache = ya.file_cache(job)
	if not cache or fs.cha(cache) then
		return true
	end

	local tmp_pdf, err = self:doc2pdf(job)
	if not tmp_pdf then
		return true, Err("    " .. "%s", err)
	end

	local output, err = Command("pdftoppm")
		:args({
			"-singlefile",
			"-jpeg",
			"-jpegopt",
			"quality=" .. PREVIEW.image_quality,
			"-f",
			1,
			tostring(tmp_pdf),
		})
		:stdout(Command.PIPED)
		:stderr(Command.PIPED)
		:output()

	local rm_tmp_pdf, rm_err = fs.remove("file", Url(tmp_pdf))
	if not rm_tmp_pdf then
		return true, Err("Failed to remove %s, error: %s", tmp_pdf, rm_err)
	end

	if not output then
		return true, Err("Failed to start `pdftoppm`, error: %s", err)
	elseif not output.status.success then
		local pages = tonumber(output.stderr:match("the last page %((%d+)%)")) or 0
		if job.skip > 0 and pages > 0 then
			ya.manager_emit("peek", { math.max(0, pages - 1), only_if = job.file.url, upper_bound = true })
		end
		return true, Err("Failed to convert %s to image, stderr: %s", tmp_pdf, output.stderr)
	end

	return fs.write(cache, output.stdout)
end

return M
